module Macro where

import           Data.List (foldl')
import           Data.Map  (Map)
import qualified Data.Map  as M

data Token = Leaf String
           | Node String [Token]
           | Newline
           deriving (Eq, Ord, Show)

type Name = String

data Macro = Def Name [Macro] Macro
           | App Name [Macro]
           | Tok Token
           deriving (Eq, Ord, Show)

-- | Spthy files
type Doc = [Macro]

-- | <name> => (<args>, body)
type Stack = Map Name ([Macro], Macro)

-----------------------------------------------------------
-- Macro processing
-----------------------------------------------------------
expand :: (Stack, Doc) -> Macro -> (Stack, Doc)
-- Adds a macro definition to the stack
expand (stack, doc) (Def name param body) =
    (M.insert name (param, body) stack, doc)

-- Lookup macro definition in the stack and substitute
expand (stack, doc) app@(App name args) = (stack, m:doc)
    where
        m = case name `M.lookup` stack of
              Nothing            -> app
              Just (param, body) -> subst param args body

-- Otherwise, put it in the accumulator
expand (stack, doc) x = (stack, x:doc)

-- | @param@ should be a list of leaves
-- TODO: ensure that it is recursive
subst :: [Macro] -> [Macro] -> Macro -> Macro
subst param args body
  | length param == length args =
      applySubst body (M.fromList $ zip param args)
    -- mismatched arity, return the body unchanged
  | otherwise = body
    where
        applySubst :: Macro -> Map Macro Macro -> Macro
        applySubst = undefined

preprocess :: Doc -> Doc
preprocess = snd . foldl' expand (M.empty, [])
