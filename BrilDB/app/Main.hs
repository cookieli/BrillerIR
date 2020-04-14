module Main where

import BrilTypes
import CommandParser
import Control.Monad.Trans.State
import System.Environment(getArgs)
import Data.ByteString.Lazy as BS hiding (head, intercalate, putStrLn, tail)
import Data.Aeson
import System.IO as IO
import Data.Map as Map
import Control.Monad.IO.Class
import Control.Monad(when)
import Data.Sequence as Seq
import Data.Maybe(fromMaybe, fromJust)
import Control.Lens hiding (List)
import Data.List(intercalate)
main :: IO ()
main = do 
        arg <- getArgs
        jsonString <- case arg of 
                      [x] -> BS.readFile x
                      _   -> errorWithoutStackTrace "usage BrilDB <filename>"
        let program =  case eitherDecode jsonString of
                    Right p -> p 
                    Left _ -> errorWithoutStackTrace "not right contents"
        hSetBuffering stdout NoBuffering
        evalStateT debugLoop $ intialState program

intialState :: Program -> DebugState
intialState p = DebugState (functions p) [intialFuncState "main"]
intialFuncState :: String -> FunctionState
intialFuncState f = FunctionState f 0 Map.empty
debugLoop :: StateT DebugState IO()
debugLoop = do 
              liftIO $ IO.putStr "(BrilDB) "
              cmd <- liftIO getLine
              when (not $ Prelude.null cmd) $ case parseCommand cmd of 
                                      Right c -> executeCommand c 
                                      Left  s -> liftIO $ IO.putStrLn s
              debugLoop
                                          
executeCommand::Command -> StateT DebugState IO()
executeCommand c = case c of 
                   Run                -> run Nothing
                   Step 1             -> step True
                   Step n             -> run (Just n)
                   List               -> list
                   Print var          -> printVar var
                   Scope              -> scopeCmd
                   Breakpoint pos cond -> setBreakPoint pos cond
                   Unknown    cmd     -> liftIO $ putStrLn $ "unknown cmd " ++ cmd

nextInstruction :: DebugState -> Instruction
nextInstruction st = case Map.lookup funcName prog of
                     Just f -> Seq.index (body f) (position funcSt)
                     Nothing -> error $ "call of unknown function " ++ funcName
     where
        prog     = program st
        funcSt   = Prelude.head $ callStack st
        funcName = functionName funcSt

setBreakPoint :: Either String Int -> BoolExpr -> StateT DebugState IO()
setBreakPoint (Right pos) cond = modify $
                                   set (_program . ix "main" . _body . ix (pred pos) . _breakCondition) cond
setBreakPoint (Left lb) cond =  do
                             pos <- gets $ (Map.lookup lb) . labels .(Map.!"main").program 
                             case pos of 
                               Nothing -> liftIO $ putStrLn $ "label: " ++ lb ++ "not existed"
                               Just v  -> setBreakPoint (Right (succ v))  cond
                               
evalBoolExpr :: Map.Map String BrilValue -> BoolExpr -> Either String Bool
evalBoolExpr m e = case e of 
                    (BoolVar v)  -> case Map.lookup v m of 
                                     Nothing -> Left $ "unbound variable: " ++ v 
                                     Just (BrilBool b) -> Right b
                                     Just (BrilInt i)  -> Left $ "it's not bool expr " ++ show i
                    (BoolConst t) -> Right t 
                    (EqOp e1 e2)  -> (==)  <$> (evalIntExpr m e1) <*> (evalIntExpr m e2)
                    (LtOp e1 e2)  -> (<)   <$> (evalIntExpr m e1) <*> (evalIntExpr m e2)
                    (GtOp e1 e2)  -> (>)   <$> (evalIntExpr m e1) <*> (evalIntExpr m e2)
                    (LeOp e1 e2)  -> (<=)  <$> (evalIntExpr m e1) <*> (evalIntExpr m e2)
                    (GeOp e1 e2)  -> (>=)  <$> (evalIntExpr m e1) <*> (evalIntExpr m e2)
                    (NotOp e1)    -> (not) <$> (evalBoolExpr m e1)
                    (AndOp e1 e2) -> (&&)  <$> (evalBoolExpr m e1) <*> (evalBoolExpr m e2)
                    (OrOp e1 e2)  -> (||)  <$> (evalBoolExpr m e1) <*> (evalBoolExpr m e2)
                    
evalIntExpr :: Map.Map String BrilValue -> IntExpr -> Either String Int 
evalIntExpr m e = case e of 
                     (IntVar v) -> case Map.lookup v m of 
                                      Nothing -> Left $ "unbound variable: " ++ v 
                                      Just (BrilInt b)   -> Right b
                                      Just (BrilBool i)  -> Left $ "it's not bool expr " ++ (show i)
                     (IntConst c)  -> Right c 
                     (AddOp a1 a2) -> (+)   <$> (evalIntExpr m a1) <*> (evalIntExpr m a2)
                     (SubOp a1 a2) -> (-)   <$> (evalIntExpr m a1) <*> (evalIntExpr m a2)
                     (MulOp a1 a2) -> (*)   <$> (evalIntExpr m a1) <*> (evalIntExpr m a2)
                     (DivOp a1 a2) -> (div) <$> (evalIntExpr m a1) <*> (evalIntExpr m a2)
                     
scopeCmd :: StateT DebugState IO()
scopeCmd = checkTerminated $
           do assoc <- gets $ Map.toList . variables . head . callStack
              mapM_ (\(k, v) -> liftIO $ putStrLn $ (show k)++" : " ++ (show v)) assoc

printVar :: String -> StateT DebugState IO()
printVar var = checkTerminated $
                 do value <- getVariable var
                    case value of
                       Just v -> liftIO  $ putStrLn (show v)
                       Nothing -> liftIO $ putStrLn "undefined"

list :: StateT DebugState IO()
list = checkTerminated $
       do Function n b _ <- gets currentFunction
          pos <- gets $ position . head. callStack
          let aux i inst =
                          if pos == i then
                             "--> "++(show i) ++" "++ (show inst) ++"\n"
                          else
                             "    "++(show i) ++ " "++ (show inst) ++ "\n"
          let funcString = Prelude.concat [n, "{\n", Seq.foldMapWithIndex aux b, "}"]
          liftIO $ putStrLn funcString
          
          
atBreakPoint :: StateT DebugState IO Bool 
atBreakPoint = do 
                   done <- gets terminated
                   if done then 
                      return False
                   else do
                      inP <- gets $  breakCondition . nextInstruction
                      vars    <- gets $ variables.head. callStack
                      let boolVal = evalBoolExpr vars inP
                      case boolVal of 
                         Right t -> return t
                         Left  _ -> return False

run :: (Maybe Int) -> StateT DebugState IO()
run (Just 0 ) = return ()
run n = checkTerminated $ do step False
                             done <- gets terminated
                             isBreak <- atBreakPoint
                             st      <- get
                             if not done && isBreak then
                                liftIO $ print $ nextInstruction st
                             else 
                                run $ fmap pred n

step :: Bool -> StateT DebugState IO()
step printStmt = checkTerminated $
                   do 
                     inst <- gets nextInstruction
                     when printStmt $ liftIO $ print inst
                     operation (fromMaybe "label" $ BrilTypes.op inst) inst
                   
checkTerminated :: StateT DebugState IO() ->  StateT DebugState IO()
checkTerminated st = do isTerminated <- gets terminated
                        if isTerminated then
                          liftIO $ putStrLn "program terminated."
                        else 
                          st 
terminated :: DebugState -> Bool 
terminated = Prelude.null . callStack

operation :: String -> Instruction -> StateT DebugState IO()
operation "const" = constOp
operation "add"   = numericalBinOp (+)
operation "sub"   = numericalBinOp (-)
operation "mul"   = numericalBinOp (*)
operation "div"   = numericalBinOp div
operation "eq"    = numericalComp (==)
operation "lt"    = numericalComp (<)
operation "gt"    = numericalComp (>)
operation "le"    = numericalComp (<=)
operation "ge"    = numericalComp (>=)
operation "and"   = booleanBinOp (&&)
operation "or"    = booleanBinOp (||)
operation "print" = printOp
operation "ret"   = const retOp
operation "label" = const incrementPosition
operation "nop"   = const incrementPosition
operation "not"   = notBoolOp (not)
operation "jmp"   = jumpLabel
operation "br"    = branchOp

constOp :: Instruction -> StateT DebugState IO()
constOp inst = do
                  setVariable (fromJust $ dest inst) (fromJust $ value inst)
                  incrementPosition
                  
printOp:: Instruction -> StateT DebugState IO()
printOp inst = do xs <- mapM getVariable' $ args inst
                  liftIO $ putStrLn $ intercalate " " $ Prelude.map show xs
                  incrementPosition

numericalBinOp :: (Int -> Int -> Int) -> Instruction -> StateT DebugState IO()
numericalBinOp f = binOp $ (\(BrilInt x) (BrilInt y) -> BrilInt $ f x y)

numericalComp :: (Int -> Int -> Bool) -> Instruction -> StateT DebugState IO()
numericalComp f = binOp $ (\(BrilInt x) (BrilInt y) -> BrilBool $ f x y)

booleanBinOp :: (Bool -> Bool -> Bool) -> Instruction -> StateT DebugState IO()
booleanBinOp f = binOp $ (\(BrilBool x) (BrilBool y) -> BrilBool $ f x y)

notBoolOp :: (Bool -> Bool) -> Instruction -> StateT DebugState IO()
notBoolOp f = unaryOp $ (\(BrilBool x) -> BrilBool $ f x)

binOp :: (BrilValue -> BrilValue -> BrilValue) -> Instruction -> StateT DebugState IO()
binOp f inst = do [x, y] <- mapM getVariable' $ args inst
                  setVariable (fromJust $ dest inst) $ f x y
                  incrementPosition

unaryOp :: (BrilValue -> BrilValue) -> Instruction -> StateT DebugState IO()
unaryOp f inst = do [x] <- mapM getVariable' $ args inst
                    setVariable (fromJust $ dest inst) $ f x
                    incrementPosition

retOp :: StateT DebugState IO()
retOp =  modify $ over _callStack tail 

jumpLabel :: Instruction -> StateT DebugState IO()
jumpLabel inst = gotoLabel $ head $ args inst

gotoLabel :: String -> StateT DebugState IO()
gotoLabel name = do func <- gets currentFunction
                    let pos = labels func Map.! name
                    modify $
                       over (_callStack . _head . _position) $ \ _ ->pos

branchOp :: Instruction -> StateT DebugState IO()
branchOp inst = do
                   let [condVar, trueLabel, falseLabel] = args inst
                   (BrilBool cond) <- getVariable' condVar
                   if cond then
                       gotoLabel trueLabel
                   else
                       gotoLabel falseLabel

getVariable :: String -> StateT DebugState IO (Maybe BrilValue)
getVariable x = gets $ Map.lookup x . variables . head. callStack        

getVariable' :: String -> StateT DebugState IO BrilValue
getVariable' x = gets $ (Map.! x) . variables . head. callStack

setVariable :: String -> BrilValue -> StateT DebugState IO()
setVariable x var = modify $
        over (_callStack . _head . _variables ) $ (Map.insert x var)
        

incrementPosition :: StateT DebugState IO()
incrementPosition = do 
                       pos <- gets $ position. head. callStack 
                       numInsts <- gets $ Seq.length .body . currentFunction
                       if numInsts == (succ pos) then
                         retOp
                       else 
                         modify $
                          over (_callStack . _head . _position) succ
                          
currentFunction :: DebugState -> Function
currentFunction (DebugState p (f:_)) = p Map.! (functionName f)
                          




