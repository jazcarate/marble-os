module AppSpec where

import           Test.Hspec

spec :: Spec
spec = do
  it "works" $ do
    True `shouldBe` True