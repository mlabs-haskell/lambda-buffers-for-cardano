mod cli;
mod query_utxos;
mod scripts;

use clap::{ArgMatches};
use query_utxos::{query_utxos};

use tx_bakery::chain_query::{Network};
use url::{Url};

#[tokio::main]
async fn main() {
    let matches = cli::cli().get_matches();

    match matches.subcommand() {
        Some(("query-utxos", sub_matches)) =>{
            if let (Some(network), Some(ogmios_url), Some(addr), option_datum) = 
                        ( sub_matches.get_one::<Network>("network")
                        , sub_matches.get_one::<Url>("ogmios_url")
                        , sub_matches.get_one::<plutus_ledger_api::v1::address::Address>("bech32-address")
                        , sub_matches.get_one::<plutus_ledger_api::plutus_data::PlutusData>("datum")
                        )
            {
                query_utxos(network.clone(), ogmios_url.clone(), addr.clone(), option_datum.clone().cloned()).await;
            }
        }
        Some(("scripts", sub_matches)) => {
            match sub_matches.subcommand() {
                Some(("eq-validator", sub_matches)) => {
                        if sub_matches.get_flag("address") {
                            println!("YES")
                        }else {
                            println!("NO")
                        }
                }
                _ => unreachable!(),

            }
        }
        Some(("build-and-submit", sub_matches)) =>{
        }
        _ => unreachable!(),
    }
    println!("Hello, world!");

}
