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
            if let (Some(network), Some(ogmios_url), Some(addr), option_datum) = (
                sub_matches.get_one::<Network>("network"),
                sub_matches.get_one::<Url>("ogmios_url"),
                sub_matches.get_one::<plutus_ledger_api::v1::address::Address>("address"),
                sub_matches.get_one::<plutus_ledger_api::plutus_data::PlutusData>("datum"),
            ) {
                query_utxos(
                    network.clone(),
                    ogmios_url.clone(),
                    addr.clone(),
                    option_datum.clone().cloned(),
                )
                .await;
            }
        }
        Some(("addresses", sub_matches)) => match sub_matches.subcommand() {
            Some(("eq-validator", _sub_matches)) => {
                let config = config::get_config().await;
                addresses::addresses_eq_validator(config);
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
