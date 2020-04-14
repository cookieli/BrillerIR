{-# LANGUAGE OverloadedStrings #-}

module BrilTypes where
  import Data.Char(toLower)
  import Data.Aeson
  import Control.Lens
  import Data.Sequence as Seq
  import Data.List(intercalate)
  import Data.Maybe (fromJust, fromMaybe, isJust)
  import qualified Data.Map.Lazy as Map
  import Data.Foldable(toList)
  
  newtype Program = Program { functions :: Map.Map String Function}
         deriving Show
  instance FromJSON Program where
       parseJSON = withObject "Program" $ \v ->
           Program . Map.fromList . map (\f -> (name f, f)) <$>
           v .: "functions"

  
  data DebugState = DebugState {
      program :: Map.Map String Function,
      callStack :: [FunctionState]
  }
  _callStack :: Lens' DebugState [FunctionState]
  _callStack = lens callStack $ \ds cs -> ds { callStack = cs}
  
  _program :: Lens' DebugState (Map.Map String Function )
  _program = lens program $ \ds pg -> ds {program = pg}
  
  
  data FunctionState = FunctionState {
     functionName :: String,
     position :: Int,
     variables :: Map.Map String BrilValue
  }
  
  _variables:: Lens' FunctionState (Map.Map String BrilValue)
  _variables = lens variables $ \fs var -> fs { variables = var }
  _position :: Lens' FunctionState Int 
  _position = lens position $ \fs v -> fs {position = v}
   
  
  data Function = Function {
      name :: String,
      body :: Seq.Seq Instruction,
      labels:: Map.Map String Int
  }
  _body :: Lens' Function (Seq.Seq Instruction)
  _body = lens body $ \func bd -> func { body = bd}

  instance Show Function where
     show (Function n b _) = n ++ " {\n" ++
         foldr (\i s ->
               if isJust (label i) then
                  show  i ++ "\n" ++ s
               else
                  " " ++ show i ++ "\n" ++ s
         ) "}" b

  instance FromJSON Function where
     parseJSON = withObject "Function" $ \v -> do
         n <- v .: "name"
         insts <- v .: "instrs"
         let labelMap =
                         Map.fromListWithKey (\l -> error ("repeated label: " ++ l)) $
                         map (over _1 (fromJust . label)) $
                         Prelude.filter (isJust . label . fst) $
                         Prelude.zip (toList insts) [0 ..]
         return $ Function n insts labelMap

  data Instruction = Instruction {
      breakCondition :: BoolExpr,
      label :: Maybe String,
      op :: Maybe String,
      dest :: Maybe String,
      typ :: Maybe Type,
      value::Maybe BrilValue,
      args ::[String]
  }
  
  _breakCondition :: Lens' Instruction BoolExpr
  _breakCondition = lens breakCondition $ \inst bc -> inst {breakCondition = bc}

  instance Show Instruction where 
      show inst = show (classify inst) ++ showBP (breakCondition inst)
         where 
           showBP (BoolConst True) = "true"
           showBP (BoolConst False)  = " (False)"
           showBP e = " (B: " ++ show e ++ ")"
           
  instance FromJSON Instruction where 
     parseJSON = withObject "Instruction" $ \v -> Instruction (BoolConst False)
        <$> v .:? "label"
        <*> v .:? "op"
        <*> v .:? "dest"
        <*> v .:? "type"
        <*> v .:? "value"
        <*> (fromMaybe [] <$> v .:? "args")
        
--  _breakCondition :: Lens' Instruction BoolExpr
  
  data ClassifiedInst = 
       Label String 
     | Const String Type BrilValue
     | ValueOp String Type String [String]
     | EffectOp String [String]
     
  instance Show ClassifiedInst where 
     show (Label l) = l ++ ":"
     show (BrilTypes.Const d t v) = 
        d ++ ": " ++ show t ++ " = const " ++ show v ++ ";"
     show (ValueOp d t o as) =
        d ++ ": " ++ show t ++ " = " ++  intercalate " " (o:as) ++ ";"
     show (EffectOp o as) = 
        intercalate " " (o:as) ++ ";"
        
        
  classify :: Instruction -> ClassifiedInst
  classify (Instruction b l o d t v a) = 
       if isJust l then
           Label $ fromJust l
       else if o == Just "const" then
           BrilTypes.Const (fromJust d) (fromJust t) (fromJust v)
       else if isJust d then 
           ValueOp (fromJust d) (fromJust t) (fromJust o) a 
       else 
           EffectOp (fromJust o) a
           
  data Type = IntType | BoolType
  
  instance Show Type where 
      show IntType  = "int"
      show BoolType = "bool"
  instance FromJSON Type where
     parseJSON = withText "Type" $ \s -> case s of 
        "int"  -> return IntType
        "bool" -> return BoolType
        _      -> fail "Type" 
  
  
  data BrilValue = BrilInt Int | BrilBool Bool
  instance Show BrilValue where
     show (BrilInt x)  = show x
     show (BrilBool x) = map toLower $ show x
     
  instance FromJSON BrilValue where 
     parseJSON v@(Number _) = BrilInt <$> parseJSON v 
     parseJSON v@(Bool _)   = BrilBool <$> parseJSON v 
     parseJSON _ = fail "Value"
     
  data Command = 
      Run
    | Step Int
    | Restart
    | Print String
    | Scope
    | Assign String BrilValue
    | Breakpoint (Either String Int) BoolExpr
    | List
    | Unknown String
    
  data BoolExpr =
       BoolVar String
     | BoolConst Bool
     | EqOp IntExpr IntExpr
     | LtOp IntExpr IntExpr
     | GtOp IntExpr IntExpr
     | LeOp IntExpr IntExpr
     | GeOp IntExpr IntExpr
     | NotOp BoolExpr
     | AndOp BoolExpr BoolExpr
     | OrOp  BoolExpr BoolExpr

  instance Show BoolExpr where
     showsPrec p e = case e of
        BoolVar v -> showString v
        BoolConst b -> showString $ map toLower $ show b
        EqOp e1 e2  -> showBinop "eq "  e1 e2
        LtOp e1 e2  -> showBinop "lt "  e1 e2
        GtOp e1 e2  -> showBinop "gt"   e1 e2
        LeOp e1 e2  -> showBinop "le"   e1 e2
        GeOp e1 e2  -> showBinop "ge"   e1 e2
        NotOp e1    -> showUnop  "not " e1
        AndOp e1 e2 -> showBinop "and " e1 e2
        OrOp  e1 e2 -> showBinop "or "  e1 e2
      where
        showBinop op e1 e2 = showParen (p > 10) $
           showString op . showsPrec 11 e1 .
           showString " " . showsPrec 11 e2
        showUnop op e1 = showParen (p > 10) $
                  showString op . showsPrec 11 e1
  data IntExpr = 
       IntVar String
     | IntConst Int
     | AddOp IntExpr IntExpr
     | MulOp IntExpr IntExpr
     | SubOp IntExpr IntExpr
     | DivOp IntExpr IntExpr

  instance Show IntExpr where
     showsPrec p e = case e of
        IntVar v -> showString v
        IntConst i -> showString (show i)
        AddOp e1 e2 -> showBinop "add " e1 e2
        SubOp e1 e2 -> showBinop "sub " e1 e2
        MulOp e1 e2 -> showBinop "mul"  e1 e2
        DivOp e1 e2 -> showBinop "div " e1 e2
      where
        showBinop op e1 e2 = showParen (p > 10) $
           showString op . showsPrec 11 e1 .
           showString " " . showsPrec 11 e2


     

