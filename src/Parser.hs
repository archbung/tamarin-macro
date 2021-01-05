module Parser (doc, tok) where

import           Control.Applicative    (liftA2)
import           Data.Char              (isAlpha, isSpace)
import           Text.Parsec            (Parsec, alphaNum, between, many, oneOf,
                                         skipMany, try, (<|>))
import           Text.Parsec.Char       hiding (spaces)
import           Text.Parsec.Combinator (sepBy)

import           Macro                  (Doc (..), Token (..))


type Parser = Parsec String ()

-----------------------------------------------------------
-- Basic parsers
-----------------------------------------------------------
spaces :: Parser ()
spaces = skipMany simpleSpace
    where
        -- | All whitespace character except newline and CRLF
        -- FIXME: Might not be enough
        simpleSpace = satisfy isSpace'
        isSpace' c
          | c == '\r' || c == '\n' = False
          | otherwise = isSpace c

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

ident :: Parser String
ident = lexeme ident'
    where
        ident' = liftA2 (:) identStart (many identLetter)
        identStart = alphaNum <|> oneOf "_~:!$#*+<=>-@?|\\^[]%.\""
        identLetter = identStart

symbol :: String -> Parser String
symbol str = lexeme (string str)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = spaces *> ident

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (symbol ",")


-----------------------------------------------------------
-- Token parsers
-----------------------------------------------------------
leaf :: Parser Token
leaf = Leaf <$> identifier

args :: Parser [Token]
args = parens (commaSep tok)  -- arg list can be empty

node :: Parser Token
node = Node <$> identifier <*> args

nl :: Parser Token
nl = Newline <$ (newline <|> crlf)

tok :: Parser Token
tok = try node <|> try leaf <|> nl

doc :: Parser Doc
doc = many tok
