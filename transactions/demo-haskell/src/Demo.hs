{- | Module: Demo

Exports the 'main' function to run the program
-}
module Demo (main) where

import Control.Monad qualified as Monad
import Data.ByteString qualified as ByteString
import Demo.Cli qualified
import Demo.Config qualified
import Demo.Process qualified
import LambdaBuffers.Runtime.Prelude qualified
import Options.Applicative qualified

main :: IO ()
main = do
  config <- Demo.Config.getDemoConfig
  demoRequest <- Monad.join $ Options.Applicative.execParser Demo.Cli.parser

  let resp = Demo.Process.process config demoRequest

  ByteString.putStr $ LambdaBuffers.Runtime.Prelude.toJsonBytes resp
