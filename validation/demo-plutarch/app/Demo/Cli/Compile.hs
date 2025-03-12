module Demo.Cli.Compile (CompileOpts (..), CompileMode (..), compile) where

import Data.ByteString qualified
import Data.ByteString.Short (fromShort)
import Demo.Validation (eqValidator)
import LambdaBuffers.Demo.Config (Config (Config, config'eqValidator), Script (Script))
import LambdaBuffers.Runtime.Prelude (toJsonBytes)
import Plutarch.Internal.Term qualified as Term (Config (NoTracing, Tracing), LogLevel (LogInfo), TracingMode (DoTracing), compile)
import Plutarch.Script (serialiseScript)

data CompileMode = COMPILE_PROD | COMPILE_DEBUG deriving stock (Show, Read, Eq)

data CompileOpts = CompileOpts
  { co'Mode :: CompileMode
  , co'File :: FilePath
  }
  deriving stock (Show, Eq)

compile :: CompileOpts -> IO ()
compile opts = do
  let cfg = case co'Mode opts of
        COMPILE_PROD -> Term.NoTracing
        COMPILE_DEBUG -> Term.Tracing Term.LogInfo Term.DoTracing

  eqValidatorCompiled <-
    either
      (\err -> fail $ "Failed compiling eqValidator with " <> show err)
      pure
      (Term.compile cfg eqValidator)

  let config =
        toJsonBytes $
          Config
            { config'eqValidator = Script (fromShort . serialiseScript $ eqValidatorCompiled)
            }
  Data.ByteString.writeFile (co'File opts) config
