{- | Module: Demo

Exports the 'main' function to run the program
-}
module Demo (main) where

import qualified Control.Monad as Monad
import qualified Demo.Cli
import qualified Demo.Config
import qualified Demo.Process
import qualified Options.Applicative

main :: IO ()
main = do
  config <- Demo.Config.getDemoConfig
  demoRequest <- Monad.join $ Options.Applicative.execParser Demo.Cli.parser

  let resp = Demo.Process.process demoRequest

  undefined
