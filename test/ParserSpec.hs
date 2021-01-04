module ParserSpec (spec) where

import           Macro       (Doc (..), Token (..))
import           Parser
import           Test.Hspec
import           Text.Parsec (parse)

spec :: Spec
spec = do
  describe "tok" $ do
    it "parses toy tokens" $ do
      parse tok "" "foo" `shouldBe` 
        Right (Leaf "foo")

      parse tok "" "baz(a,foo(c,d))" `shouldBe` 
        Right (Node "baz" [Leaf "a", Node "foo" [Leaf "c", Leaf "d"]]) 

  describe "doc" $ do
    it "parses simple documents" $ do
      parse doc "" "foo baz bar" `shouldBe`
        Right [Leaf "foo", Leaf "baz", Leaf "bar"]

      parse doc "" "foo(a,b) bar baz(baz(g))" `shouldBe`
        Right [Node "foo" [Leaf "a", Leaf "b"], Leaf "bar", Node "baz" [Node "baz" [Leaf "g"]]]

    it "parses tamarin-style docs" $ do
      parse doc "" "rule foo: [ Fr(~x) ] --> [ Out(~x) ]" `shouldBe`
        Right [Leaf "rule", Leaf "foo:", Leaf "[", Node "Fr" [Leaf "~x"], Leaf "]", Leaf "-->", Leaf "[", Node "Out" [Leaf "~x"], Leaf "]"]
