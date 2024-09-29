{- | Module: Demo.Config

Utilities for grabbing the LB Demo.Config from the environment
-}
module Demo.Config (getDemoConfig) where

import qualified Control.Monad.Except as Except
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import LambdaBuffers.Demo.Config (Config (..))
import qualified System.Environment as Environment

{- | Gets the 'Config' by reading the file in the @DEMO_CONFIG@ environment
variable
-}
getDemoConfig :: IO Config
getDemoConfig = do
  demoConfigFilePath <- Environment.lookupEnv "DEMO_CONFIG"
  ByteString.readFile
    Monad.>=> Except.liftEither
    . Bifunctor.first IO.Error.userError
    . LambdaBuffers.Runtime.Prelude.fromJsonBytes
    $ demoConfigFilePath
