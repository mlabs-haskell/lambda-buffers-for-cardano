{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Demo.Cli.Compile (CompileOpts (..), CompileMode (..), compile) where

import Data.ByteString qualified
import Data.ByteString.Short (fromShort)
import Demo.Validation (eqValidator)
import LambdaBuffers.Demo.Config (Config (Config, config'eqValidator), Script (Script))
import LambdaBuffers.Runtime.Prelude (toJsonBytes)
import PlutusLedgerApi.Data.V3 (ScriptContext)
import PlutusLedgerApi.V3 (serialiseCompiledCode)
import PlutusTx qualified (CompiledCode, compile)
import PlutusTx.Plugin ()
import PlutusTx.Prelude (BuiltinUnit)

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

eqValidatorCompiled :: PlutusTx.CompiledCode (ScriptContext -> BuiltinUnit)
eqValidatorCompiled = $$(PlutusTx.compile [||eqValidator||])
