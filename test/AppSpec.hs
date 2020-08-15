{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import           App

spec :: Spec
spec = do
  it "works" $ do
    True `shouldBe` True