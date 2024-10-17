use std::io::Read;
use tx_bakery::chain_query::Network;
use tx_bakery::submitter::Submitter;
use tx_bakery::wallet::Wallet;
use tx_bakery_ogmios::client::{OgmiosClient, OgmiosClientConfigBuilder};
use url::Url;

/// Builds and submits a transaction using tx-village
pub async fn build_and_submit(
    config: lbf_demo_config_api::demo::config::Config,
    signing_key_file: std::path::PathBuf,
    option_staking_signing_key_file: Option<std::path::PathBuf>,
    network: Network,
    url: Url,
) -> anyhow::Result<()> {
    let eq_validator_script_or_ref =
        tx_bakery::utils::script::ScriptOrRef::from_bytes(config.eq_validator.0).expect(&format!(
            "Couldn't deserialize PlutusScript of eq_validator."
        ));
    let eq_validator = eq_validator_script_or_ref.as_validator();

    let wallet = tx_bakery::utils::key_wallet::KeyWallet::new(
        signing_key_file,
        option_staking_signing_key_file,
    )
    .await
    .map_err(|err| anyhow::anyhow!(err))?;

    let ogmios_client_config = OgmiosClientConfigBuilder::default()
        .network(network)
        .url(url)
        .build()
        .unwrap();

    let ogmios_client = OgmiosClient::connect(ogmios_client_config).await.unwrap();

    let mut std_in_contents = String::new();

    std::io::stdin().read_to_string(&mut std_in_contents)?;

    let tx_info : plutus_ledger_api::v2::transaction::TransactionInfo =
            lbr_prelude::json::Json::from_json_string(&std_in_contents)
                .expect(&format!(
                    "Expected stdin to be a LB JSON `TxInfo` object, but failed to deserialize the JSON object")
                    );

    // Creating a map of all the scripts used in the transaction (unused scripts won't be
    // attached)
    let scripts = std::collections::BTreeMap::from([eq_validator.1.with_script_hash()]);

    // Define the strategy to find a suitable collateral
    let collateral = tx_bakery::CollateralStrategy::Automatic { amount: 5_000_000 }; // TODO(jaredponn)
                                                                                     // October 17,
                                                                                     // 2024: this
                                                                                     //       should
                                                                                     //       be
                                                                                     //       adjustable
                                                                                     //       based
                                                                                     //       on a
                                                                                     //       cli
                                                                                     //       argument

    // Initialise TxBakery by fetching protocol parameters from the ChainQuery
    let tx_bakery = tx_bakery::TxBakery::init(&ogmios_client).await?;

    // Define the strategy to handle change outpu
    let change_strategy = tx_bakery::ChangeStrategy::Address(wallet.get_change_addr());

    // // Transaction with context will attach required scripts, collateral, etc.
    let tx = tx_bakery::TxWithCtx::new(&tx_info, &scripts, &collateral, &change_strategy);

    eprintln!("Baking and delivering transaction...");

    // Bake, sign and submit the transaction
    let tx_hash = tx_bakery
        .bake_and_deliver(&ogmios_client, &wallet, tx)
        .await
        .unwrap();

    eprintln!("Awaiting transaction {:?} to be confirmed...", tx_hash);
    ogmios_client.await_tx_confirm(&tx_hash).await.unwrap();

    eprintln!("Transaction has been confirmed!");

    Ok(())
}
