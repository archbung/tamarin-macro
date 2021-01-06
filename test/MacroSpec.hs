module MacroSpec where

import           Macro      (Doc (..), Macro (..), Token (..), preprocess)
import           Test.Hspec

spec :: Spec
spec = do
  describe "preprocess" $ do
    it "expands simple macros" $ do
      2+2 `shouldBe` 4
