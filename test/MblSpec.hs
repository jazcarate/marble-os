module MblSpec where

import           Test.Hspec
import           Mbl
import           Configuration
import qualified Duration                      as D
import           Data.Either                    ( isLeft )

configuration :: RunConfiguration
configuration = RunConfiguration "/foo/bar" 1 False (D.seconds 1) '-'

spec :: Spec
spec = do
  describe "Mbl" $ do
    describe "Simple" $ do
      it "can runParser waits" $ do
        runParser configuration "---" `shouldBe` Right [Wait, Wait, Wait]
      it "can runParser prints" $ do
        runParser configuration "foo" `shouldBe` Right [Print "foo"]
    describe "Combo" $ do
      it "can runParser waits and prints" $ do
        runParser configuration "-foo--bar"
          `shouldBe` Right [Wait, Print "foo", Wait, Wait, Print "bar"]
    describe "Edge case" $ do
      it "fail when parsing empty string" $ do
        runParser configuration "" `shouldSatisfy` isLeft
    describe "Configuration" $ do
      it "can change delimiter" $ do
        runParser (configuration { delimiter = 'x' }) "xx-foo-x"
          `shouldBe` Right [Wait, Wait, Print "-foo-", Wait]
    describe "Escaped" $ do
      it "can print the delimiter if escaped with \\" $ do
        runParser configuration "--this is a\\-somewhat\\-convoluted example"
          `shouldBe` Right
                       [ Wait
                       , Wait
                       , Print "this is a-somewhat-convoluted example"
                       ]
      it "needs to escape the escape" $ do
        runParser configuration "--\\\\--"
          `shouldBe` Right [Wait, Wait, Print "\\", Wait, Wait]
    describe "multi-line" $ do
      it "chooses the line configured" $ do
        runParser configuration { lane = 2 } "1\n2" `shouldBe` Right [Print "2"]
    describe "references" $ do
      it "replaces the reference with the word" $ do
        runParser configuration "1\n[1]: foo" `shouldBe` Right [Print "foo"]
      it "ignores additional references" $ do
        runParser configuration "1\n[2]: foo" `shouldBe` Right [Print "1"]
      it "uses the last reference" $ do
        runParser configuration "1\n[1]: foo\n[1]: bar"
          `shouldBe` Right [Print "bar"]
      it "ignores white-spaces" $ do
        runParser configuration "1\n[1] :   foo" `shouldBe` Right [Print "foo"]
      it "can do multi-line" $ do
        runParser configuration "1\n[1]: foo\nbar[2]: Biz"
          `shouldBe` Right [Print "foo\nbar"]

