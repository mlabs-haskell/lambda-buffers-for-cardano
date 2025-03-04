use plutus_ledger_api::v3::transaction::TransactionInfo;
use std::io::Read;
use tx_bakery::submitter::Submitter;
use tx_bakery::utils::key_wallet::KeyWallet;
use tx_bakery::wallet::Wallet;
use tx_bakery::{chain_query::Network, utils::script::ScriptOrRef};
use tx_bakery::{ChangeStrategy, CollateralStrategy, TxBakery, TxWithCtx};
use tx_bakery_ogmios::client::{OgmiosClient, OgmiosClientConfigBuilder};
use url::Url;

/// Builds and submits a transaction using tx-village
pub async fn build_and_submit(
    config: lbf_demo_config_api::demo::config::Config,
    signing_key_file: std::path::PathBuf,
    option_staking_signing_key_file: Option<std::path::PathBuf>,
    network: Network,
    url: Url,
) {
    let eq_validator_script_or_ref = ScriptOrRef::from_bytes_v3(config.eq_validator.0)
        .unwrap_or_else(|err| {
            panic!(
                "Couldn't deserialize PlutusScript of eq_validator with error {}",
                err
            )
        });
    let eq_validator = eq_validator_script_or_ref.as_validator();

    let wallet = KeyWallet::new(signing_key_file, option_staking_signing_key_file)
        .await
        .unwrap_or_else(|err| panic!("Failed to create KeyWallet with error {}", err));

    let ogmios_client_config = OgmiosClientConfigBuilder::default()
        .network(network)
        .url(url)
        .build()
        .unwrap_or_else(|err| panic!("Ogmios client config failed with error {}", err));

    let ogmios_client = OgmiosClient::connect(ogmios_client_config).await.unwrap();

    let mut std_in_contents = String::new();

    std::io::stdin()
        .read_to_string(&mut std_in_contents)
        .unwrap_or_else(|err| panic!("Failed to read `stdin` to a string with error {}", err));

    let tx_info : TransactionInfo =
            lbr_prelude::json::Json::from_json_string(&std_in_contents)
                .unwrap_or_else( |err| panic!("Expected stdin to be a LB JSON `TxInfo` object, but failed to deserialize the JSON object with error {}", err));

    // Creating a map of all the scripts used in the transaction (unused scripts won't be
    // attached)
    let scripts = std::collections::BTreeMap::from([eq_validator.1.with_script_hash()]);

    // TODO(jaredponn) October 17, 2024: this should be adjustable based on a cli argument
    // Define the strategy to find a suitable collateral
    let collateral = CollateralStrategy::Automatic {
        min_amount: 5_000_000,
        max_utxo_count: 1,
    };

    // Initialise TxBakery by fetching protocol parameters from the ChainQuery
    let tx_bakery = TxBakery::init(&ogmios_client)
        .await
        .unwrap_or_else(|err| panic!("TxBakery failed to initialize with error {}", err));

    // Define the strategy to handle change outpu
    let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());

    // Transaction with context will attach required scripts, collateral, etc.
    let tx = TxWithCtx::new(&tx_info, &scripts, &collateral, &change_strategy);

    eprintln!("Baking and delivering transaction...");

    // Bake, sign and submit the transaction
    let tx_hash = tx_bakery
        .bake_and_deliver(&ogmios_client, &wallet, tx)
        .await
        .unwrap_or_else(|err| panic!("Failed to bake and deliver transaction with error {}", err));

    eprintln!("Awaiting transaction {} to be confirmed...", tx_hash);

    ogmios_client.await_tx_confirm(&tx_hash).await.unwrap();

    eprintln!("Transaction {} has been confirmed!", tx_hash);

    let tx_hash_lb_json_str = plutus_ledger_api::json::Json::to_json_string(&tx_hash);

    print!("{}", tx_hash_lb_json_str);
}
