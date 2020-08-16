module Spec where

import Test.Hspec

import qualified AppSpec
import qualified DurationSpec
import qualified MblSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "App"     AppSpec.spec
  describe "Duration" DurationSpec.spec
  describe "Mbl"     MblSpec.spec