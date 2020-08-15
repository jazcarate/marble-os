module MblSpec where

import           Test.Hspec
import           Mbl
import           Mbl.Types

-- TODO spaces between

spec :: Spec
spec = do
  describe "Mbl" $ do
    describe "Simple" $ do
      it "can parse waits" $ do
        parseMbl "---" `shouldBe` Right [Wait, Wait, Wait]
      it "can parse prints" $ do
        parseMbl "foo" `shouldBe` Right [Print "foo"]
    describe "Combo" $ do
      it "can parse waits and prints" $ do
        parseMbl "-foo--bar"
          `shouldBe` Right [Wait, Print "foo", Wait, Wait, Print "bar"]
    describe "Edge case" $ do
      it "parses empty string ok" $ do
        parseMbl ""
          `shouldBe` Right []
    describe "Escaped" $ do
      it "can print the delimiter if escaped with \\ " $ do
        parseMbl "--this is a\\-somewhat\\-convoluted example"
          `shouldBe` Right [Wait, Wait, Print "this is a-somewhat-convoluted example"]
