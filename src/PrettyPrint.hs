module PrettyPrint where

import           Data.List (intersperse)
import           Macro     (Doc (..), Macro (..), Token (..))

pp' :: Macro -> String
pp' = undefined

-- | FIXME: this will create lots of trailing space
pp :: Doc -> String
pp = unwords . map pp'
