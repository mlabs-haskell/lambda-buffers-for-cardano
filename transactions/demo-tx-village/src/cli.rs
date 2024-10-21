use clap::value_parser;
use clap::{Arg, Args, Command};
use tx_bakery::clap::KeyWalletOpts;
use tx_bakery_ogmios::clap::OgmiosOpts;

use std::io::{Error, ErrorKind};

// The cli parser for interacting with the system
pub fn cli() -> Command {
    Command::new("demo-tx-village")
        .about("A CLI interface to interact with the demo protocol")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            OgmiosOpts::augment_args(
            KeyWalletOpts::augment_args(
            Command::new("build-and-submit")
                .about("Builds and submits a transaction with a provided `Request<_>` on STDIN outputting the LB JSON encoded TransactionHash on STDOUT")
                )
        )
        )
        .subcommand(
            OgmiosOpts::augment_args(
                Command::new("query-utxos")
                    .about("Query UTxOs related to the protocol with the output as LB encoded JSON on STDOUT")
                    .arg(Arg::new("address")
                            .help("Bech32 address to query with")
                            .long("address")
                            .value_parser(clap::builder::ValueParser::new(parse_bech32))
                            .required(true)
                         )
                    .arg(Arg::new("eq-datum")
                            .help("Filepath of an LB encoded JSON EqDatum that must reside as an inline datum at the UTxO")
                            .long("lb-json-eq-datum-filepath")
                            .value_parser(value_parser!(String))
                         )
                )
                .arg_required_else_help(true),
        )
        .subcommand(
            Command::new("addresses")
                .flatten_help(true)
                .subcommand_required(true)
                .subcommand(Command::new("eq-validator")
                            .about("Return the human readable bech32 encoded address for the eq-validator")
                            .arg(Arg::new("network")
                                    .help("The Cardano network to use for the address (mainnet | testnet)")
                                    .long("network")
                                    .value_parser(["mainnet", "testnet"])
                                    .default_value("testnet")
                                    .required(true)
                                 )
                    )
        )
}

// Parses an address from a bech32 string
fn parse_bech32(str: &str) -> Result<plutus_ledger_api::v1::address::Address, std::io::Error> {
    let result: Result<
        cardano_serialization_lib::address::Address,
        cardano_serialization_lib::error::JsError,
    > = cardano_serialization_lib::address::Address::from_bech32(str);

    match result {
        Ok(csl_addr) => {
            let try_from_csl_result: Result<
                plutus_ledger_api::v1::address::Address,
                tx_bakery::utils::csl_to_pla::TryFromCSLError,
            > = tx_bakery::utils::csl_to_pla::TryFromCSL::<
                cardano_serialization_lib::address::Address,
            >::try_from_csl(&csl_addr);
            match try_from_csl_result {
                Ok(pla_addr) => Ok(pla_addr),
                Err(try_from_csl_err) => Err(Error::new(
                    ErrorKind::InvalidInput,
                    try_from_csl_err.to_string(),
                )),
            }
        }
        Err(csl_js_err) => Err(Error::new(ErrorKind::InvalidInput, csl_js_err.to_string())),
    }
}
