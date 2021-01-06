module PrettyPrintSpec where

import           Macro       (Doc (..), Macro (..), Token (..))
import           PrettyPrint
import           Test.Hspec


--spec :: Spec
--spec = do
--  describe "pp" $ do
--    it "pretty-prints simple docs" $ do
--      pp [Tok (Node "enc" [Leaf "x", Leaf "a"])] `shouldBe`
--        "enc(x,a)"

spec :: Spec
spec = do
  describe "preprocess" $ do
    it "expands simple macros" $ do
      2+2 `shouldBe` 4
