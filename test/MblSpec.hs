module MblSpec where

import           Test.Hspec
import           Mbl
import           Configuration
import           Duration

configuration :: Configuration
configuration = Configuration "/foo/bar" False (seconds 1) '-'

spec :: Spec
spec = do
  describe "Mbl" $ do
    describe "Simple" $ do
      it "can parse waits" $ do
        parse configuration "---" `shouldBe` Right [Wait, Wait, Wait]
      it "can parse prints" $ do
        parse configuration "foo" `shouldBe` Right [Print "foo"]
    describe "Combo" $ do
      it "can parse waits and prints" $ do
        parse configuration "-foo--bar"
          `shouldBe` Right [Wait, Print "foo", Wait, Wait, Print "bar"]
    describe "Edge case" $ do
      it "parses empty string ok" $ do
        parse configuration "" `shouldBe` Right []
    describe "Configuration" $ do
      it "can change delimiter" $ do
        parse (configuration { delimiter = 'x' }) "xx-foo-x"
          `shouldBe` Right [Wait, Wait, Print "-foo-", Wait]
    describe "Escaped" $ do
      it "can print the delimiter if escaped with \\ " $ do
        parse configuration "--this is a\\-somewhat\\-convoluted example"
          `shouldBe` Right
                       [ Wait
                       , Wait
                       , Print "this is a-somewhat-convoluted example"
                       ]
