module Parse where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char


type Parser = Parsec Void Text

parseCommand :: Text -> Either (ParseError (Token Text) Void) (Text, [Text])
parseCommand msg = parse commandParser "" msg


backslash, singleQuote, doubleQuote :: Char
backslash = '\\'
singleQuote = '\''
doubleQuote = '"'


commandParser :: Parser (Text, [Text])
commandParser = do
  space
  cmd <- some $ noneOf " /"
  space
  args <- (quoted singleQuote <|> quoted doubleQuote <|> some (noneOf " "))
          `sepEndBy` space
  return (T.pack cmd, map T.pack args)


escaped :: Char -> Parser Char
escaped q = char backslash *> (char backslash <|> char q)


quoted :: Char -> Parser String
quoted q = between (char q) (char q) $ many (try (escaped q) <|> noneOf [q])
