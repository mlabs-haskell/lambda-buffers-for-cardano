{- | Module: Demo.Config

Utilities for grabbing the LB Demo.Config from the environment
-}
module Demo.Config (getDemoConfig) where

import Control.Exception qualified as Exception
import Control.Monad.Except qualified as Except
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString qualified as ByteString
import LambdaBuffers.Demo.Config (Config (..))
import LambdaBuffers.Runtime.Prelude qualified
import System.Environment qualified as Environment
import System.IO.Error qualified as IO.Error

{- | Gets the 'Config' by reading the file in the @DEMO_CONFIG@ environment
variable
-}
getDemoConfig :: IO Config
getDemoConfig = do
  demoConfigFilePath <-
    Environment.lookupEnv "DEMO_CONFIG"
      >>= \case
        Nothing -> Exception.throwIO $ IO.Error.userError "Environment variable `DEMO_CONFIG` was unset, and expected a file path"
        Just result -> return result
  demoConfigFileContents <- ByteString.readFile demoConfigFilePath
  Except.liftEither
    . Bifunctor.first IO.Error.userError
    . LambdaBuffers.Runtime.Prelude.fromJsonBytes
    $ demoConfigFileContents
