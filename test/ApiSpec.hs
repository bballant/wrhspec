{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ApiSpec (spec) where

{-|
 # Testing in haskell with Hspec
-}

import qualified Data.Aeson
import qualified Data.Map.Lazy  as M
import           Import
import           Network.Wai
import           RIO.Process
import           Run
import           Test.Hspec
import           Test.Hspec.Wai
import           WaiRIO

testApp
  :: FilePath
  -> Map Integer Integer
  -> IO App
testApp cachePath cacheVals = do
  _  <-  Data.Aeson.encodeFile cachePath cacheVals
  lo <- logOptionsHandle stderr (optionsVerbose testOptions)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    return App
             { appLogFunc = lf
             , appProcessContext = pc
             , appOptions = testOptions
           }
  where
    testOptions =
      Options
        { optionsVerbose   = True
        , optionsPort      = 8080 -- unused in tests
        , optionsCachePath = cachePath
        }

testApp'
  :: Map Integer Integer
  -> IO App
testApp' =
  testApp "test/resources/cache.json"

testWaiApplication
  :: Map Integer Integer
  -> IO Application
testWaiApplication cache = do
  app <- testApp' cache
  runRIO app (runApplicationT webApp)

setupTeardownApp
  :: Map Integer Integer
  -> (Application -> IO a)
  -> IO a
setupTeardownApp cache runTestsWith = do
  myApp <- testWaiApplication cache
  -- do stuff to set up test
  finally (runTestsWith myApp)
    ( -- do stuff to clean up after test
      return ())

spec :: Spec
spec = do
  describe "App Tests" $
    before (testApp' $ M.fromList [(10,55)]) $
      it "cache can be read" $ \app -> do
        cache <- runRIO app getCache
        cache `shouldBe` M.fromList [(10,55)]
  describe "Application Tests" $
    around (setupTeardownApp $ M.fromList [(1000, 666)]) $ do
      it "responds positively" $
        get "/fib/1" `shouldRespondWith` 200
      it "gets value out of cache" $
        get "/fib/1000" `shouldRespondWith` "666"
      it "calculates small value" $
        get "/fib/10" `shouldRespondWith` "55"
