{-# LANGUAGE NumericUnderscores #-}

module DurationSpec where

import           Test.Hspec
import           Duration
import           Data.Either                    ( isLeft )

spec :: Spec
spec = do
  describe "Durations" $ do
    describe "microseconds" $ do
      it "can parse microseconds" $ do
        toMicroseconds <$> parseDuration "10us" `shouldBe` Right 10
      it "can parse big microseconds" $ do
        toMicroseconds <$> parseDuration "1000000us" `shouldBe` Right 1_000_000
    describe "milliseconds" $ do
      it "can parse milliseconds" $ do
        toMicroseconds <$> parseDuration "10ms" `shouldBe` Right 10_000
      it "can parse big milliseconds" $ do
        toMicroseconds <$> parseDuration "1000000ms" `shouldBe` Right
          1_000_000_000
    describe "seconds" $ do
      it "can parse seconds" $ do
        toMicroseconds <$> parseDuration "10s" `shouldBe` Right 10_000_000
      it "can parse big milliseconds" $ do
        toMicroseconds <$> parseDuration "1000000s" `shouldBe` Right
          1_000_000_000_000
    describe "minutes" $ do
      it "can parse minutes" $ do
        toMicroseconds <$> parseDuration "10m" `shouldBe` Right 600_000_000
      it "can parse big minutes" $ do
        toMicroseconds <$> parseDuration "1000000m" `shouldBe` Right
          60_000_000_000_000
    describe "default" $ do
      it "uses seconds as the default unit" $ do
        parseDuration "10" `shouldBe` parseDuration "10s"
    describe "wrong input" $ do
      it "fails if there are no numbers" $ do
        parseDuration "foo" `shouldSatisfy` isLeft
      it "fails if we don't know the unit" $ do
        parseDuration "10i" `shouldSatisfy` isLeft
    describe "combination" $ do
      it "can parse multiple units" $ do
        toMicroseconds <$> parseDuration "3m10s" `shouldBe` Right 190_000_000
