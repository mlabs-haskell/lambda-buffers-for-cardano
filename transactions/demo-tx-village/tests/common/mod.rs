use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer, Product, Record, Sum};
use plutus_ledger_api::v2::address::{Address, Credential};
use plutus_ledger_api::v2::crypto::LedgerBytes;
use plutus_ledger_api::v2::script::{MintingPolicyHash, ScriptHash};
use plutus_ledger_api::v2::value::{AssetClass, CurrencySymbol, TokenName};
use url::Url;
use plutus_ledger_api::v2::script::ValidatorHash;
use tx_bakery::utils::script::ScriptOrRef;
use lbf_demo_config_api::demo::config::{Config, Script};
use lbr_prelude::json::Json;
use tokio::fs;
use tx_bakery_plutip::{Plutip, PlutipConfigBuilder};
use tx_bakery_ogmios::{
    client::{OgmiosClient, OgmiosClientConfigBuilder},
    launcher::{OgmiosLauncher, OgmiosLauncherConfigBuilder},
};
use std::io::Write;
use assert_cmd::Command;
use tx_bakery::utils::key_wallet::{KeyWallet};
use tx_bakery::wallet::Wallet;

pub fn eq_validator_address(config_path: &str, network :&str) -> cardano_serialization_lib::address::Address {
    let mut cmd = Command::cargo_bin("demo-tx-village").unwrap();

    let assert = cmd
            .env("DEMO_CONFIG", config_path)
            .arg("addresses")
            .arg("eq-validator")
            .arg("--network")
            .arg(network);

    let stdout_contents = String::from_utf8(assert.assert().success().get_output().stdout.clone()).unwrap();

    cardano_serialization_lib::address::Address::from_bech32(&stdout_contents).unwrap()
}

pub fn query_utxos(config_path : &str
                        , network : &str
                        , addr : cardano_serialization_lib::address::Address
                        , option_plutus_data : Option<plutus_ledger_api::plutus_data::PlutusData> ) -> std::vec::Vec<plutus_ledger_api::v2::transaction::TxInInfo> {

    let mut cmd = Command::cargo_bin("demo-tx-village").unwrap();

    let mut temp_file = tempfile::NamedTempFile::new().unwrap();

    let mut assert = cmd
            .env("DEMO_CONFIG", config_path)
            .arg("query-utxos")
            .arg("--address")
            .arg(addr.to_bech32(None).unwrap())
            .arg("--network")
            .arg(network);

    match option_plutus_data  {
        Some(plutus_data) => {
            let json_string_plutus_data = plutus_data.to_json_string();

            temp_file.write(json_string_plutus_data.as_bytes()).unwrap();
            assert = assert.arg("--lb-json-datum-filepath")
                           .arg(temp_file.path().to_str().unwrap()) ;
        }
        None => {
        }
    }


    let stdout_contents = String::from_utf8(assert.assert().success().get_output().stdout.clone()).unwrap();

    let result = Json::from_json_string(&stdout_contents).unwrap();

    temp_file.close().unwrap();

    return result;
}

pub fn build_and_submit(config_path : &str,
                        network : &str,
                        signing_key_file: &str,
                        tx_info : plutus_ledger_api::v2::transaction::TransactionInfo
                        ) -> () {

    let mut cmd = Command::cargo_bin("demo-tx-village").unwrap();

    let assert = cmd
            .env("DEMO_CONFIG", config_path)
            .arg("build-and-submit")
            .arg("--signing-key-file")
            .arg(signing_key_file)
            .arg("--network")
            .arg(network)
            .write_stdin(tx_info.to_json_string());


    let stderr_contents = String::from_utf8(assert.assert().success().get_output().stderr.clone()).unwrap();

    eprintln!("{}", stderr_contents);
    
    return ();

}

/// Returns a triplet of:
/// - The first wallet's `.skey` path
/// - The first wallet's `KeyWallet` type
/// - The first wallet's csl `Address`
/// WARNING: this assumes that we used tx-villages plutip launcher which puts the wallets in the
/// `WALLETS_DIR` directory.
pub async fn get_the_wallet(network: u8) -> (std::path::PathBuf, KeyWallet, cardano_serialization_lib::address::Address) {
    let entries = std::fs::read_dir::<std::path::PathBuf>(WALLETS_DIR.try_into().unwrap());

    let an_skey : std::path::PathBuf = entries.unwrap().filter(|result_dir_entry| match result_dir_entry { Ok(dir_entry) => { dir_entry.path().extension() == Some(std::ffi::OsStr::new("skey")) }  Err(_) => {false}   } ).next().unwrap().expect("REASON").path();
    let key_wallet = KeyWallet::new(an_skey.clone(), None::<std::path::PathBuf>).await.unwrap();
    let key_wallet_change_pkh = key_wallet.get_change_pkh().0.0;

    ( an_skey.clone()
     , key_wallet
     , cardano_serialization_lib::address::EnterpriseAddress::new(network, &cardano_serialization_lib::address::StakeCredential::from_keyhash(
             &cardano_serialization_lib::crypto::Ed25519KeyHash::from_bytes(
                 key_wallet_change_pkh
                    ).unwrap()
             )
                                                                  ).to_address() 
     )
}

/// The hardcoded path of the directory of the wallets
/// NOTE(jaredponn) October 16, 2024: This matches the hardcoded values in tx-village
/// 
pub static WALLETS_DIR : &str =  ".wallets";

/// Creates the test data of
///
/// 1. The datum and redeemer for a sequence of txes which test if the input is equal
///
/// 2. The datum and redeemer for a sequence of txes which test if the input is not equal
///
/// NOTE(jaredponn): October 15,2024. This matches the test data in the CTL implementation
pub fn setup_test_data(validator_hash: ValidatorHash) -> ((EqDatum, EqRedeemer), (EqDatum, EqRedeemer)) {
    let example_token_name = TokenName::from_string("example token name");
    let example_currency_symbol =
        CurrencySymbol::NativeToken(MintingPolicyHash(ScriptHash(LedgerBytes([0].repeat(28)))));

    let example_asset_class = AssetClass {
        currency_symbol: example_currency_symbol,
        token_name: example_token_name,
    };
    let example_plutus_bytes = LedgerBytes(b"example bytes".to_vec());

    let example_address = Address {
        credential: Credential::Script(validator_hash),
        staking_credential: None,
    };

    let example_eq_datum_a = EqDatum {
        rec: Record {
            bar: example_address.clone(),
            baz: example_plutus_bytes.clone(),
            foo: example_asset_class.clone(),
        },
        sum: Sum::Baz(example_plutus_bytes.clone()),
        prod: Product(
            example_asset_class.clone(),
            example_address.clone(),
            example_plutus_bytes.clone(),
        ),
    };

    let example_eq_redeemer_a = EqRedeemer::IsEqual( example_eq_datum_a.clone() ); 

    let example_eq_datum_b = EqDatum {
        rec: Record {
            bar: example_address.clone(),
            baz: example_plutus_bytes.clone(),
            foo: example_asset_class.clone(),
        },
        sum: Sum::Foo(example_asset_class.clone()),
        prod: Product(example_asset_class, example_address, example_plutus_bytes),
    };

    let example_eq_redeemer_b = EqRedeemer::IsNotEqual( example_eq_datum_a.clone() ); 

    ( (example_eq_datum_a, example_eq_redeemer_a)
    , (example_eq_datum_b, example_eq_redeemer_b)
    )
}

pub async fn read_script(path: &str) -> ScriptOrRef {
    let conf_str = fs::read_to_string(path).await.expect(&format!(
        "Couldn't read plutarch config JSON file at {}.",
        path
    ));

    let conf: Config = Json::from_json_string(&conf_str)
        .expect(&format!("Couldn't deserialize JSON data of file {}", path));

    let Script(raw_script) = conf.eq_validator;

    ScriptOrRef::from_bytes(raw_script).expect(&format!(
        "Couldn't deserialize PlutusScript of file {}.",
        path
    ))
}

pub async fn setup_plutip_test() -> (Plutip, OgmiosLauncher, OgmiosClient) {
    let plutip_config = PlutipConfigBuilder::default()
            ._utxos(5)
            .build().unwrap();
    let plutip = Plutip::start(plutip_config).await.unwrap();

    let ogmios_config = OgmiosLauncherConfigBuilder::default()
        .node_socket(plutip.get_node_socket())
        .node_config(plutip.get_node_config_path().await)
        .build()
        .unwrap();
    let ogmios_launcher = OgmiosLauncher::start(ogmios_config).await.unwrap();

    let ogmios_client_config = OgmiosClientConfigBuilder::default()
        .network(plutip.get_network())
        .url(Url::parse("http://127.0.0.1:1337").unwrap())
        .build()
        .unwrap();
    let ogmios_client = OgmiosClient::connect(ogmios_client_config).await.unwrap();

    (plutip, ogmios_launcher, ogmios_client)
}
