{-# LANGUAGE NumericUnderscores #-}

module MblSpec where

import           Test.Hspec
import           Mbl
import           Configuration
import qualified Duration                      as D
import           Data.Either                    ( isLeft )
import           Prelude                 hiding ( repeat )

configuration :: ParseConfiguration
configuration = ParseConfiguration '-' '|' (Numbered 1) Nothing Nothing Nothing

defaultWait :: D.Duration
defaultWait = D.seconds 1
threeSeconds :: D.Microseconds
threeSeconds = D.toMicroseconds $ D.seconds 3
defaultSeconds :: D.Microseconds
defaultSeconds = D.toMicroseconds $ defaultWait

aWait :: Action
aWait = Wait { implicit = False, time = defaultSeconds }

aImplicitWait :: Action
aImplicitWait = aWait { implicit = True }

spec :: Spec
spec = do
  describe "Mbl Actions" $ do
    let parse conf content = actions <$> runParser conf content
    describe "Simple" $ do
      it "can parse waits" $ do
        parse configuration "-" `shouldBe` Right [aWait]
      it "can parse prints" $ do
        parse configuration "foo" `shouldBe` Right [Print "foo"]
    describe "Combo" $ do
      it "waits after a print if there is something else" $ do
        parse configuration "foo-"
          `shouldBe` Right [Print "foo", aImplicitWait, aWait]
      it "can parse waits and prints" $ do
        parse configuration "-foo--bar" `shouldBe` Right
          [aWait, Print "foo", aImplicitWait, aWait, aWait, Print "bar"]
    describe "Edge case" $ do
      it "fail when parsing empty string" $ do
        parse configuration "" `shouldSatisfy` isLeft
    describe "Configuration" $ do
      it "can change delimiter" $ do
        parse (configuration { delimiter = 'x' }) "xx-foo-x"
          `shouldBe` Right [aWait, aWait, Print "-foo-", aImplicitWait, aWait]
      it "can change split" $ do
        parse (configuration { split = 'x' }) "fooxbar"
          `shouldBe` Right [Print "foo", aImplicitWait, Print "bar"]
    describe "Escaped" $ do
      it "can print the delimiter if escaped with \\" $ do
        parse configuration "--this is a\\-somewhat\\-convoluted example"
          `shouldBe` Right
                       [ aWait
                       , aWait
                       , Print "this is a-somewhat-convoluted example"
                       ]
      it "needs to escape the escape" $ do
        parse configuration "--\\\\--" `shouldBe` Right
          [aWait, aWait, Print "\\", aImplicitWait, aWait, aWait]
    describe "multi-line" $ do
      it "chooses the line configured" $ do
        parse configuration { lane = Numbered 2 } "1\n2"
          `shouldBe` Right [Print "2"]
      it "ignores empty newlines" $ do
        parse configuration { lane = Numbered 2 } "1\n\n2"
          `shouldBe` Right [Print "2"]
    describe "references" $ do
      it "replaces the reference with the word" $ do
        parse configuration "1\n[1]: foo" `shouldBe` Right [Print "foo"]
      it "ignores additional references" $ do
        parse configuration "1\n[2]: foo" `shouldBe` Right [Print "1"]
      it "uses the last reference" $ do
        parse configuration "1\n[1]: foo\n[1]: bar"
          `shouldBe` Right [Print "bar"]
      it "ignores white-spaces" $ do
        parse configuration "1\n[1] :   foo" `shouldBe` Right [Print "foo"]
      it "can do multi-line" $ do
        parse configuration "1\n[1]:\nfoo\\\nbar\n[2]: Biz"
          `shouldBe` Right [Print "foo\nbar"]
      it "interprets a ref as a standalone tick" $ do
        parse configuration "12\n[1]:foo\n[2]: bar"
          `shouldBe` Right [Print "foo", aImplicitWait, Print "bar"]
      it "assumes print if the ir no ref" $ do
        parse configuration "12\n[1]:foo"
          `shouldBe` Right [Print "foo", aImplicitWait, Print "2"]
      it "matches the longest ref" $ do
        parse configuration "12\n[1]:foo\n[12]: bar"
          `shouldBe` Right [Print "bar"]
    describe "tick rate" $ do
      it "can interpret inline tick rate" $ do
        parse configuration "tick: 3s\n-"
          `shouldBe` Right [Wait { implicit = False, time = threeSeconds }]
      it "configuration overrides tick-rate" $ do
        parse
            configuration { tickRateOverride = Just $ TickRate $ D.seconds 3 }
            "tick: 2m\n-"
          `shouldBe` Right [Wait { implicit = False, time = threeSeconds }]
      it "fails with multiple tick rates" $ do
        parse configuration "tick: 3s\ntick: 2s-" `shouldSatisfy` isLeft
    describe "Repeat strategy symbols" $ do
      it "ignores any repeat strategy symbol not in the beginning" $ do
        parse configuration "->>" `shouldBe` Right [aWait, Print ">>"]
      it "ignores spaces between repeat strategy and rest" $ do
        parse configuration ">3  --" `shouldBe` Right [aWait, aWait]
    describe "splits" $ do
      it "can split a print with a special character" $ do
        parse configuration "2|3"
          `shouldBe` Right [Print "2", aImplicitWait, Print "3"]
      it "the split can be escaped" $ do
        parse configuration "2\\|3" `shouldBe` Right [Print "2|3"]
      it "a ref can't be split" $ do
        parse configuration "1\n[1]: 2|3" `shouldBe` Right [Print "2|3"]
  describe "Mbl Name" $ do
    let parse conf content = name <$> runParser conf content
    it "an mbl can be named" $ do
      parse configuration "foo: 1" `shouldBe` Right (Just "foo")
    it "strips whitespace" $ do
      parse configuration "foo\t  : 1" `shouldBe` Right (Just "foo")
    it "can be unnamed and no harm no foul" $ do
      parse configuration "1" `shouldBe` Right Nothing
    it "chooses the correctly named lane" $ do
      parse configuration { lane = Named "bar" } "foo: 1\nbar: 2"
        `shouldBe` Right (Just "bar")
    it "fails when no lanes are named that way" $ do
      parse configuration { lane = Named "bar" } "foo: 1"
        `shouldBe` Left "No lane with the name \"bar\""
  describe "Mbl Repeat" $ do
    let parse conf content = repeat <$> runParser conf content
    it "the default repeat is to play it once" $ do
      parse configuration "--" `shouldBe` Right Once
    it "It can loop indefinitely" $ do
      parse configuration "|--" `shouldBe` Right Infinite
    it "Can be repeated once" $ do
      parse configuration ">--" `shouldBe` Right (Repeat 1)
    it "Can be repeated some times" $ do
      parse configuration ">>--" `shouldBe` Right (Repeat 2)
    it "can also have a number" $ do
      parse configuration ">3--" `shouldBe` Right (Repeat 3)
    it "symbols in the middle are not counted to the repeat strategy" $ do
      parse configuration "|>>-foo-bar" `shouldBe` Right Infinite
    it "symbols can be escaped" $ do
      parse configuration "\\|--" `shouldBe` Right Once
    it "can be overridden in the configuration" $ do
      parse configuration { repeatStrategyOverride = Just Once } "|--"
        `shouldBe` Right Once
  describe "Mbl Show" $ do
    describe "Single Mbl" $ do
      it "shows one char mbl inline" $ do
        showMBL <$> runParser configuration "tick: 1s\n--1-2" `shouldBe` Right
          "tick: 1s\n--a-b\n\n[a]: 1\n[b]: 2"
      it "shows the split" $ do
        showMBL <$> runParser configuration "tick: 1s\n--a|b" `shouldBe` Right
          "tick: 1s\n--ab\n\n[a]: a\n[b]: b"
      it "escapes delimiters" $ do
        showMBL <$> runParser configuration "tick: 1s\n--\\-" `shouldBe` Right
          "tick: 1s\n--a\n\n[a]: -"
      it "keeps the name" $ do
        showMBL <$> runParser configuration "tick: 1s\nfoo: 1" `shouldBe` Right
          "tick: 1s\nfoo: a\n\n[a]: 1"
      it "shows multiple char in ref" $ do
        showMBL <$> runParser configuration "tick: 1s\n--foo-" `shouldBe` Right
          "tick: 1s\n--a-\n\n[a]: foo"
      it "shows single ref when repeated" $ do
        showMBL
          <$>        runParser configuration "tick: 1s\n--foo-foo"
          `shouldBe` Right "tick: 1s\n--a-a\n\n[a]: foo"
    describe "Multiple Mbls" $ do
      it "shows one char mbl inline" $ do
        showMBLs
            [ MBL
              Nothing
              [ Print "foo"
              , aImplicitWait
              , aWait
              , Print "bar"
              , aImplicitWait
              , aWait
              , Print "foo"
              ]
              Once
            , MBL
              Nothing
              [ Print "foo"
              , aWait { time = D.Microseconds 3000000 }
              , Print "biz"
              ]
              Once
            ]
          `shouldBe` "tick: 1s\na-b-a\na---c\n\n[a]: foo\n[b]: bar\n[c]: biz"
      it "align names" $ do
        showMBLs
            [ MBL
              (Just "name")
              [ Print "foo"
              , aImplicitWait
              , aWait
              , Print "bar"
              , aImplicitWait
              , aWait
              , Print "foo"
              ]
              Once
            , MBL
              Nothing
              [ Print "foo"
              , Wait { implicit = False, time = D.Microseconds 3000000 }
              , Print "biz"
              ]
              Once
            ]
          `shouldBe` "tick: 1s\nname: a-b-a\n      a---c\n\n[a]: foo\n[b]: bar\n[c]: biz"
