{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_wrhspec

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_wrhspec.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> option auto ( long "port"
                      <> short 'p'
                      <> metavar "PORT"
                      <> help "Wai server's port"
                       )
       <*> strOption ( long "filepath"
                      <> short 'f'
                      <> metavar "PORT"
                      <> value "./"
                      <> help "FilePath"
                       )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
