{-# LANGUAGE NoImplicitPrelude #-}
module FibSpec (spec) where

{-|
 # Testing in haskell with Hspec
-}

import Import
import Test.Hspec
import Test.QuickCheck
import Fib
import qualified Data.Map.Lazy as M

basicFib
  :: Integer
  -> Integer
basicFib 0 = 0
basicFib 1 = 1
basicFib n =
  basicFib (n-1) + basicFib (n-2)

spec :: Spec
spec =
  describe "Fib" $ do
    -- we understand fibonacci
    -- run `stack test`
    it "calculates basicFib 5" $
      basicFib 5 `shouldBe` 5
    it "calculates basicFib 11" $
      basicFib 10 `shouldBe` 55

    -- unit test cachedFib function
    it "calculates cachedFib with empty cache" $
      cachedFib 11 M.empty `shouldBe` 89
    it "calculates cachedFib with good cache" $
      cachedFib 11 (M.fromList [(11, 89)]) `shouldBe` 89

    -- stack test --file-watch
    it "reads from cache even if value is wrong" $
      cachedFib 11 (M.fromList [(11, 3)]) `shouldBe` 3

    -- stack test --ta '--match "Fib/works with any cached number"'
    -- a property can be a function that returns True/False
    -- on quickcheck https://www.fpcomplete.com/blog/2017/01/quickcheck
    it "works with any cached number" $ property
      (\(x, y) -> cachedFib x (M.fromList [(x, y)]) == y)

    -- TODO: properties with constraints
