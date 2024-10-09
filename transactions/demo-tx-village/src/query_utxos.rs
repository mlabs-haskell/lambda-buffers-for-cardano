use tx_bakery_ogmios::{
        client::{OgmiosClient, OgmiosClientConfigBuilder},
    };
use tx_bakery::chain_query::{Network,ChainQuery};

use url::{Url};

pub async fn query_utxos(network : Network, url : Url, addr :plutus_ledger_api::v1::address::Address ) 
    {
    let ogmios_client_config = OgmiosClientConfigBuilder::default()
        .network(network)
        .url(url)
        .build()
        .unwrap();
    let ogmios_client = OgmiosClient::connect(ogmios_client_config).await.unwrap();

    let utxo_lookups = ogmios_client.query_utxos_by_addr(&addr).await;

    match utxo_lookups {
        Ok(utxos) => {
        }
        Err(chain_query_err) => {
        }
    }

    return ();
}
