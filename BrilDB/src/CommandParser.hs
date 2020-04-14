module CommandParser where
import BrilTypes
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Maybe(fromMaybe)
import Text.Parsec.Combinator
import Text.Parsec hiding (runP)

tokenParser = T.makeTokenParser emptyDef

identifier = T.identifier tokenParser
integer    = fromInteger <$> T.integer tokenParser
natural    = fromInteger <$> T.natural tokenParser
parens     = T.parens tokenParser
reserved   = T.reserved tokenParser
whiteSpace = T.whiteSpace tokenParser
parseCommand:: String -> Either String Command
parseCommand = showLeft . parse command ""
    where
        showLeft (Left x)  = Left (show x)
        showLeft (Right x) = Right x
command = whiteSpace *> (identifier >>= continueCmd) <* eof
continueCmd s = case s of 
    "run"   -> return Run
    "step"  -> Step <$> optionOr 1 natural
    "list"  -> return List
    "print" -> Print <$> identifier
    "scope" -> return Scope
    "break" -> Breakpoint <$>lineLabel <*> (optionOr (BoolConst True) boolExpr)
    _       -> Unknown <$> identifier

lineLabel = Left <$> identifier <|> Right <$> natural

boolExpr = BoolConst <$> booConst
          <|> BoolVar <$> identifier
          <|> parens boolOp
boolOp = identifier >>= (\id -> case id of 
          "eq"  -> EqOp  <$> intExpr <*> intExpr
          "lt"  -> LtOp  <$> intExpr <*> intExpr
          "gt"  -> GtOp  <$> intExpr <*> intExpr
          "le"  -> LeOp  <$> intExpr <*> intExpr
          "ge"  -> GeOp  <$> intExpr <*> intExpr
          "not" -> NotOp <$> boolExpr
          "add" -> AndOp <$> boolExpr <*> boolExpr
          "or"  -> OrOp  <$> boolExpr <*> boolExpr
        )
           

intExpr = IntConst <$> integer
          <|> IntVar <$> identifier
          <|> parens intOp
          
intOp = identifier >>= (\id -> case id of 
           "add" -> AddOp <$> intExpr  <*> intExpr
           "sub" -> SubOp <$> intExpr  <*> intExpr
           "mul" -> MulOp <$> intExpr  <*> intExpr
           "div" -> DivOp <$> intExpr  <*> intExpr
           )
           
booConst  = string "true" *> return True
             <|> string "false" *> return False
optionOr x p = fromMaybe x <$> optionMaybe p
