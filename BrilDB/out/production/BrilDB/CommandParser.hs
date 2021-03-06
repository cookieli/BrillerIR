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
    "run"  -> return Run
    "step" -> Step <$> optionOr 1 natural
    
optionOr x p = fromMaybe x <$> optionMaybe p
