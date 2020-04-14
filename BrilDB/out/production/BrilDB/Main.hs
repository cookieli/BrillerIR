module Main where

import BrilTypes
import CommandParser
import Control.Monad.Trans.State
import System.Environment(getArgs)
import Data.ByteString.Lazy as BS hiding (head)
import Data.Aeson
import System.IO as IO
import Data.Map as Map
import Control.Monad.IO.Class
import Control.Monad(when)
import Data.Sequence as Seq
import Data.Maybe(fromMaybe, fromJust)
import Control.Lens
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
                   Run -> liftIO $ IO.putStrLn "run cmd"
                   Step 1 -> step True

nextInstruction :: DebugState -> Instruction
nextInstruction st = case Map.lookup funcName prog of
                     Just f -> Seq.index (body f) (position funcSt)
                     Nothing -> error $ "call of unknown function " ++ funcName
     where
        prog     = program st
        funcSt   = Prelude.head $ callStack st
        funcName = functionName funcSt

step :: Bool -> StateT DebugState IO()
step printStmt = do
                   inst <- gets nextInstruction
                   when printStmt $ liftIO $ print inst
                   operation (fromMaybe "label" $ BrilTypes.op inst) inst

operation :: String -> Instruction -> StateT DebugState IO()
operation "const" = constOp
operation "add"   = numericalBinOp (+)
operation "sub"   = numericalBinOp (-)
operation "mul"   = numericalBinOp (*)
operation "div"   = numericalBinOp div

constOp :: Instruction -> StateT DebugState IO()
constOp inst = do
                  setVariable (fromJust $ dest inst) (fromJust $ value inst)
                  incrementPosition
                  

numericalBinOp :: (Int -> Int -> Int) -> Instruction -> StateT DebugState IO()
numericalBinOp f = binOp $ (\(BrilInt x) (BrilInt y) -> BrilInt $ f x y)

binOp :: (BrilValue -> BrilValue -> BrilValue) -> Instruction -> StateT DebugState IO()
binOp f inst = do [x, y] <- mapM getVariable' $ args inst
                  setVariable (fromJust $ dest inst) $ f x y
                  incrementPosition
                  
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
                       modify $
                          over (_callStack . _head . _position) succ
                          




