module Parser (doc, tok) where

import           Data.Char              (isAlpha)
import           Text.Parsec            (Parsec, alphaNum, many, oneOf, try,
                                         (<|>))
import           Text.Parsec.Combinator (sepBy)
import qualified Text.Parsec.Language   as L
import qualified Text.Parsec.Token      as T

import           Macro                  (Doc (..), Token (..))


type Parser = Parsec String ()

lang :: T.TokenParser ()
lang = T.makeTokenParser $ L.emptyDef
    { T.identStart = alphaNum <|> oneOf "_~:!$#*+<=>-@?|\\^[]%.\""
    , T.identLetter = alphaNum <|> oneOf "_~:!$#*+<=>-@?|\\^[]%.\""
    }

identifier :: Parser String
identifier = space *> T.identifier lang  -- skips leading whitespace

parens :: Parser a -> Parser a
parens = T.parens lang

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep lang

space :: Parser ()
space = T.whiteSpace lang

leaf :: Parser Token
leaf = Leaf <$> identifier

args :: Parser [Token]
args = parens (commaSep tok)  -- arg list can be empty

node :: Parser Token
node = Node <$> identifier <*> args

tok :: Parser Token
tok = try node <|> leaf

doc :: Parser Doc
doc = many tok
