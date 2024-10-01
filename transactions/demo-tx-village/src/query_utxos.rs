use tx_bakery::chain_query::{ChainQuery, Network};
use tx_bakery_ogmios::client::{OgmiosClient, OgmiosClientConfigBuilder};
use url::Url;

/// Queries UTxOs relevant for the demo protocol.
/// More precisely, this does the following:
///     - Queries by an address
///     - If the a datum is provided, filters the UTxOs at the above address by returning only the
///     UTxOs which have an inline datum that matches the provided datum.
pub async fn query_utxos(
    network: Network,
    url: Url,
    addr: plutus_ledger_api::v1::address::Address,
    option_datum: Option<plutus_ledger_api::plutus_data::PlutusData>,
) {
    let ogmios_client_config = OgmiosClientConfigBuilder::default()
        .network(network)
        .url(url)
        .build()
        .unwrap();
    let ogmios_client = OgmiosClient::connect(ogmios_client_config).await.unwrap();

    let utxo_lookups = ogmios_client.query_utxos_by_addr(&addr).await;

    match utxo_lookups {
        Ok(mut utxos) => {
            match option_datum {
                Some(datum) => {
                    utxos.retain(|_, tx_out| {
                        if let plutus_ledger_api::v2::datum::OutputDatum::InlineDatum(
                            plutus_ledger_api::v2::datum::Datum(inline_datum),
                        ) = &tx_out.datum
                        {
                            datum == *inline_datum
                        } else {
                            false
                        }
                    });
                }
                None => {}
            };

            let txin_infos: std::vec::Vec<plutus_ledger_api::v2::transaction::TxInInfo> = utxos
                .into_iter()
                .map(|(txin, full_txout)| {
                    let txout: plutus_ledger_api::v2::transaction::TransactionOutput =
                        plutus_ledger_api::v2::transaction::TransactionOutput::from(full_txout);
                    return plutus_ledger_api::v2::transaction::TxInInfo::from((txin, txout));
                })
                .collect();

            let txin_infos_lb_json = plutus_ledger_api::json::Json::to_json(&txin_infos);

            println!("{}", txin_infos_lb_json.as_str().unwrap());

            return ();
        }
        Err(chain_query_err) => {
            panic!("Chain query error: {}", chain_query_err)
        }
    }
}
