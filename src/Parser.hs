module Parser where

import           Data.Char              (isAlpha)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator

import           Macro           (Doc (..), Token (..))


type Parser = Parsec String ()

identifier :: Parser String
identifier = many1 $ satisfy (\c -> isAlpha c || elem c "!#~&$[]//-<>:")

leaf :: Parser Token
leaf = Leaf <$> identifier

node :: Parser Token
node = do 
    i <- identifier
    spaces
    -- FIXME: it should be allowed to have spaces between ','
    args <- between (char '(') (char ')') (tok `sepBy` char ',')
    return $ Node i args

tok :: Parser Token
tok = try node <|> leaf

doc :: Parser Doc
doc = tok `sepBy` space
