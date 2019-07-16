{-# LANGUAGE NoImplicitPrelude #-}
module Fib where

import Import
import qualified Data.Map.Lazy as M

cachedFib
  :: Integer
  -> Map Integer Integer
  -> Integer
-- cachedFib 0 _ = 0
-- cachedFib 1 _ = 1
cachedFib 0 cache = M.findWithDefault 0 0 cache
cachedFib 1 cache = M.findWithDefault 1 1 cache
cachedFib n cache =
  M.findWithDefault recurse n cache
  where recurse =
          cachedFib (n-1) cache
          + cachedFib (n-2) cache
