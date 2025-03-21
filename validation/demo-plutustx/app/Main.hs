module Main (main) where

import Demo.Cli.Compile (CompileMode (COMPILE_DEBUG), CompileOpts (CompileOpts), compile)

import Control.Applicative ((<**>))
import Data.Kind (Type)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  command,
  customExecParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  prefs,
  progDesc,
  showDefault,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
  subparser,
  value,
 )

type Command :: Type
newtype Command
  = Compile CompileOpts

compileOpts :: Parser CompileOpts
compileOpts =
  CompileOpts
    <$> option
      auto
      ( long "mode"
          <> metavar "COMPILE_MODE"
          <> help "Mode of compilation COMPILE_DEBUG|COMPILE_PROD"
          <> value COMPILE_DEBUG
          <> showDefault
      )
    <*> strOption
      ( long "file"
          <> metavar "COMPILE_FILE"
          <> help "A JSON file to store the compiled scripts"
          <> value "demo-config.json"
          <> showDefault
      )

options :: Parser Command
options =
  subparser $
    command
      "compile"
      (info (Compile <$> compileOpts <* helper) (progDesc "Compile scripts and write them to a file"))

parserInfo :: ParserInfo Command
parserInfo = info (options <**> helper) (fullDesc <> progDesc "Demo PlutusTx CLI")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Compile opts -> compile opts
