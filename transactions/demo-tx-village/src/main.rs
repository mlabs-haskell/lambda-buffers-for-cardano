mod addresses;
mod build_and_submit;
mod cli;
mod config;
mod query_utxos;

use build_and_submit::build_and_submit;
use query_utxos::query_utxos;

use tx_bakery::chain_query::Network;
use url::Url;

#[tokio::main]
async fn main() {
    // let config = config::get_config().await;

    let matches = cli::cli().get_matches();

    match matches.subcommand() {
        Some(("query-utxos", sub_matches)) => {
            if let (Some(network), Some(ogmios_url), Some(addr), option_datum_path) = (
                sub_matches.get_one::<Network>("network"),
                sub_matches.get_one::<Url>("ogmios_url"),
                sub_matches.get_one::<plutus_ledger_api::v1::address::Address>("address"),
                sub_matches.get_one::<String>("datum"),
            ) {
                query_utxos(
                    network.clone(),
                    ogmios_url.clone(),
                    addr.clone(),
                    option_datum_path.cloned(),
                )
                .await;
            }
        }
        Some(("addresses", sub_matches)) => match sub_matches.subcommand() {
            Some(("eq-validator", sub_sub_matches)) => {
                let config = config::get_config().await;
                if let Some(network) = sub_sub_matches.get_one::<String>("network") {
                    // TODO(jaredponn): October 10, 2024: Recall `1` is for the mainnet and `0`
                    // would be the testnet. We really should use tx_bakery::chain_query::Network
                    // i.e., we should write a clap parser for the type
                    // `tx_bakery::chain_query::Network` and update tx-village upstream
                    let network_u8 = if network == "mainnet" {
                        1
                    } else if network == "testnet" {
                        0
                    } else {
                        panic!("invalid network value of {}", network)
                    };
                    addresses::addresses_eq_validator(config, network_u8);
                }
            }
            _ => unreachable!(),
        },
        Some(("build-and-submit", sub_matches)) => {
            let config = config::get_config().await;
            if let (
                Some(signing_key_file),
                option_staking_signing_key_file,
                Some(network),
                Some(ogmios_url),
            ) = (
                sub_matches.get_one::<std::path::PathBuf>("signing_key_file"),
                sub_matches.get_one::<std::path::PathBuf>("staking_signing_key_file"),
                sub_matches.get_one::<Network>("network"),
                sub_matches.get_one::<Url>("ogmios_url"),
            ) {
                let _ = build_and_submit(
                    config,
                    signing_key_file.clone(),
                    option_staking_signing_key_file.cloned(),
                    network.clone(),
                    ogmios_url.clone(),
                )
                .await;
            }
        }
        _ => unreachable!(),
    }
}
