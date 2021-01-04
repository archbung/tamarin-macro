module Macro where

import           Control.Monad.State
import           Data.Map                  (Map)
import qualified Data.Map                  as M

data Token = Leaf String
           | Node String [Token]
           deriving (Eq, Ord, Show)

type Doc = [Token]

-- | <name> => (<args>, body)
type S = Map String ([String], String)

run :: Doc -> State S Doc
run = traverse go
    where
        go :: Token -> State S Token
        go (Node n args) = undefined
        go x = pure x

preprocess :: Doc -> Doc
preprocess doc = evalState (run doc) M.empty
