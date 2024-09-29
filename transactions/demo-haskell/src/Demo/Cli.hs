{- | Module: Demo.Cli

Provides a CLI interface for interacting with the demo protocol
-}
module Demo.Cli (parser) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import LambdaBuffers.Demo.Request (ClaimRequest, DemoRequest (..), LockRequest, Request (..))
import LambdaBuffers.Runtime.Prelude (Json)
import qualified LambdaBuffers.Runtime.Prelude
import Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative
import qualified System.IO.Error as IO.Error

-- | The CLI parser
parser :: ParserInfo (IO DemoRequest)
parser =
  Options.Applicative.info
    ( Options.Applicative.subparser
        ( Options.Applicative.command "build-tx-info" buildTxInfoRequest
        -- <> Options.Applicative.command "scripts" lockCommand
        )
    )
    (Options.Applicative.progDesc "CLI interface for interacting with the demo protocol")

buildTxInfoRequest :: ParserInfo (IO DemoRequest)
buildTxInfoRequest =
  Options.Applicative.info
    ( Options.Applicative.subparser
        ( Options.Applicative.command "lock" (fmap DemoRequest'Lock <$> lockRequest)
            <> Options.Applicative.command "claim" (fmap DemoRequest'Claim <$> claimRequest)
        )
    )
    (Options.Applicative.progDesc "Creates a TxInfo suitable for tx-village's bakery to build / submit a transaction")

claimRequest :: ParserInfo (IO (Request ClaimRequest))
claimRequest =
  Options.Applicative.info
    claimRequestFilePathParser
    (Options.Applicative.progDesc "Creates a TxInfo suitable for tx-village's bakery for creating a Lock transaction")

claimRequestFilePathParser :: Parser (IO (Request ClaimRequest))
claimRequestFilePathParser =
  readAndLbJsonParse
    <$> Options.Applicative.strOption
      ( Options.Applicative.long "request"
          <> Options.Applicative.metavar "FILEPATH"
          <> Options.Applicative.help "Filepath of a LB JSON (Request ClaimRequest)"
      )

lockRequest :: ParserInfo (IO (Request LockRequest))
lockRequest =
  Options.Applicative.info
    lockRequestFilePathParser
    (Options.Applicative.progDesc "Creates a TxInfo suitable for tx-village's bakery for creating a Lock transaction")

lockRequestFilePathParser :: Parser (IO (Request LockRequest))
lockRequestFilePathParser =
  readAndLbJsonParse
    <$> Options.Applicative.strOption
      ( Options.Applicative.long "request"
          <> Options.Applicative.metavar "FILEPATH"
          <> Options.Applicative.help "Filepath of a LB JSON (Request LockRequest)"
      )

-- | Reads, then parses using LB's Json instances
readAndLbJsonParse :: (Json a) => FilePath -> IO a
readAndLbJsonParse =
  ByteString.readFile
    Monad.>=> Except.liftEither
      . Bifunctor.first IO.Error.userError
      . LambdaBuffers.Runtime.Prelude.fromJsonBytes
