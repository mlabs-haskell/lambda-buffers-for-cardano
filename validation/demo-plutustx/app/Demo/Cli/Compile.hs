{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Demo.Cli.Compile (CompileOpts (..), CompileMode (..), compile) where

import Data.ByteString qualified
import Data.ByteString.Short (fromShort)
import Demo.Validation (eqValidator)
import LambdaBuffers.Demo.Config (Config (Config, config'eqValidator), Script (Script))
import LambdaBuffers.Runtime.Prelude (toJsonBytes)
import PlutusLedgerApi.V2 (serialiseCompiledCode)
import PlutusTx qualified (BuiltinData, CompiledCode, compile)
import PlutusTx.Plugin ()

data CompileMode = COMPILE_PROD | COMPILE_DEBUG deriving stock (Show, Read, Eq)

data CompileOpts = CompileOpts
  { co'Mode :: CompileMode
  , co'File :: FilePath
  }
  deriving stock (Show, Eq)

compile :: CompileOpts -> IO ()
compile opts = do
  let config =
        toJsonBytes $
          Config
            { config'eqValidator = Script (fromShort $ serialiseCompiledCode eqValidatorCompiled)
            }
  Data.ByteString.writeFile (co'File opts) config

eqValidatorCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
eqValidatorCompiled = $$(PlutusTx.compile [||eqValidator||])
