//! Common functionality for writing tests. Namely:
//! - Running an integration test suite
//! - Wrappers for calling the `demo-tx-village` CLI to serialize and deserialize inputs / outputs
//! to a Rust type
//! - Setting up the plutip testnet along with other dependencies
use assert_cmd::Command;
use lbf_demo_config_api::demo::config::{Config, Script};
use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer, Product, Record, Sum};
use lbf_demo_plutus_api::demo::request::{ClaimRequest, LockRequest, Request};
use lbf_demo_plutus_api::demo::response::Response;
use lbr_prelude::json::Json;
use num_bigint::BigInt;
use plutus_ledger_api::v1::transaction::POSIXTime;
use plutus_ledger_api::v2::address::{Address, Credential};
use plutus_ledger_api::v2::crypto::LedgerBytes;
use plutus_ledger_api::v2::script::ValidatorHash;
use plutus_ledger_api::v2::script::{MintingPolicyHash, ScriptHash};
use plutus_ledger_api::v2::value::{AssetClass, CurrencySymbol, TokenName};
use std::io::Write;
use std::time::SystemTime;
use tokio::fs;
use tx_bakery::utils::key_wallet::KeyWallet;
use tx_bakery::utils::script::ScriptOrRef;
use tx_bakery::wallet::Wallet;
use tx_bakery_ogmios::{
    client::{OgmiosClient, OgmiosClientConfigBuilder},
    launcher::{OgmiosLauncher, OgmiosLauncherConfigBuilder},
};
use tx_bakery_plutip::{Plutip, PlutipConfigBuilder};
use url::Url;

/// Given an executable, say `myExecutable` which conforms to the CLI of
///
/// a. `myExecutable build-tx-info lock --request <request-filepath>` produces a `Response` suitable
///   for storing an EqDatum at an Eq validator
///
/// b. `myExecutable build-tx-info claim --request <request-filepath>` produces a `Response` suitable
///   for claiming an EqDatum at an Eq validator
///
/// this runs an integration test which:
///
/// 1. Runs a.
/// 2. Runs b. s.t. the claimiing of the Eq validator tests if the redeemer and the datum are the
///    same
///
/// 3. Runs a.
///
/// 4. Runs b. s.t. the claimiing of the Eq validator tests if the redeemer and the datum are NOT the
///    same
pub async fn run_integration_test(executable: &str, config_path: &str) {
    let script = read_script(config_path).await.as_validator();

    let ((example_eq_datum_a, example_eq_redeemer_a), (example_eq_datum_b, example_eq_redeemer_b)) =
        setup_test_data(script.0.clone());

    let (plutip, _ogmios_launcher, _ogmios) = setup_plutip_test().await;

    let network = plutip.get_network();

    let (skey_path, key_wallet, wallet_addr) = get_the_wallet(&network).await;

    // Create the UTxO for the lock command (storing the datum)
    // -------------------------------
    {
        eprintln!("Storing EqDatum A @ EqV");
        let change_utxos =
            demo_tx_village_query_utxos(config_path, &network, wallet_addr.clone(), None);

        let current_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_millis();

        let lock_response = cli_lock_response(
            executable,
            config_path,
            Request {
                fee_inputs: change_utxos,
                change_address: key_wallet.get_change_addr(),
                current_time: POSIXTime(BigInt::from(current_time)),
                request: LockRequest {
                    eq_datum: example_eq_datum_a.clone(),
                },
            },
        );
        let tx_hash = match lock_response {
            lbf_demo_plutus_api::demo::response::Response::Result(result) => {
                demo_tx_village_build_and_submit(
                    config_path,
                    &network,
                    skey_path.to_str().unwrap(),
                    result.tx_info,
                )
            }
            lbf_demo_plutus_api::demo::response::Response::Error(err) => {
                panic!("{:?}", err)
            }
        };

        eprintln!("Successfully stored EqDatum A @ EqV with {:?}", tx_hash);
    }

    // Create the tx for claiming the locked datum
    // -----------------------------------------
    {
        eprintln!("Checking if EqDatum A is the same as the one previously stored (it should be)");

        let change_utxos =
            demo_tx_village_query_utxos(config_path, &network, wallet_addr.clone(), None);

        let eq_validator_addr = demo_tx_village_eq_validator_address(config_path, &network);

        let utxos_for_datum = demo_tx_village_query_utxos(
            config_path,
            &network,
            eq_validator_addr.clone(),
            Some(example_eq_datum_a),
        );

        let utxo_for_datum = &utxos_for_datum[0];

        let current_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_millis();

        let claim_response = cli_claim_response(
            executable,
            config_path,
            Request {
                fee_inputs: change_utxos,
                change_address: key_wallet.get_change_addr(),
                current_time: POSIXTime(BigInt::from(current_time)),
                request: ClaimRequest {
                    locked_utxo: utxo_for_datum.clone(),
                    eq_redeemer: example_eq_redeemer_a,
                },
            },
        );
        let tx_hash = match claim_response {
            lbf_demo_plutus_api::demo::response::Response::Result(result) => {
                demo_tx_village_build_and_submit(
                    config_path,
                    &network,
                    skey_path.to_str().unwrap(),
                    result.tx_info,
                )
            }
            lbf_demo_plutus_api::demo::response::Response::Error(err) => {
                panic!("{:?}", err)
            }
        };

        eprintln!(
            "Successfully checked that they are indeed the same in transaction {:?}",
            tx_hash
        );
    }

    // Create the UTxO for the lock command (storing the datum)
    // -------------------------------
    {
        eprintln!("Storing EqDatum B @ EqVal");
        let change_utxos =
            demo_tx_village_query_utxos(config_path, &network, wallet_addr.clone(), None);

        let current_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_millis();

        let lock_response = cli_lock_response(
            executable,
            config_path,
            Request {
                fee_inputs: change_utxos,
                change_address: key_wallet.get_change_addr(),
                current_time: POSIXTime(BigInt::from(current_time)),
                request: LockRequest {
                    eq_datum: example_eq_datum_b.clone(),
                },
            },
        );
        let tx_hash = match lock_response {
            lbf_demo_plutus_api::demo::response::Response::Result(result) => {
                demo_tx_village_build_and_submit(
                    config_path,
                    &network,
                    skey_path.to_str().unwrap(),
                    result.tx_info,
                )
            }
            lbf_demo_plutus_api::demo::response::Response::Error(err) => {
                panic!("{:?}", err)
            }
        };
        eprintln!("Successfully stored EqDatum B with {:?}", tx_hash);
    }

    // Create the tx for claiming the locked datum
    // -----------------------------------------
    {
        eprintln!(
            "Checking if Eq Datum A is different to the one previously stored (it should be)"
        );

        let change_utxos =
            demo_tx_village_query_utxos(config_path, &network, wallet_addr.clone(), None);

        let eq_validator_addr = demo_tx_village_eq_validator_address(config_path, &network);

        let utxos_for_datum = demo_tx_village_query_utxos(
            config_path,
            &network,
            eq_validator_addr.clone(),
            Some(example_eq_datum_b),
        );

        let utxo_for_datum = &utxos_for_datum[0];

        let current_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_millis();

        let claim_response = cli_claim_response(
            executable,
            config_path,
            Request {
                fee_inputs: change_utxos,
                change_address: key_wallet.get_change_addr(),
                current_time: POSIXTime(BigInt::from(current_time)),
                request: ClaimRequest {
                    locked_utxo: utxo_for_datum.clone(),
                    eq_redeemer: example_eq_redeemer_b,
                },
            },
        );
        let tx_hash = match claim_response {
            lbf_demo_plutus_api::demo::response::Response::Result(result) => {
                demo_tx_village_build_and_submit(
                    config_path,
                    &network,
                    skey_path.to_str().unwrap(),
                    result.tx_info,
                )
            }
            lbf_demo_plutus_api::demo::response::Response::Error(err) => {
                panic!("{:?}", err)
            }
        };

        eprintln!(
            "Successfully check that they are indeed different in transaction {:?}",
            tx_hash
        );
    }
}

/// Calls a CLI interface with the provided executable to produce a `Response` suitable for creating a lock tx.
///
/// NOTE(jaredponn) October 21, 2024: the idea is that in the other supported LB languages, there'd
/// be an executable, say `myExecutable` which conforms to the CLI of
/// `myExecutable build-tx-info lock --request <request-filepath>`
/// to build a `Response` suitable for tx-village.
pub fn cli_lock_response(
    executable: &str,
    config_path: &str,
    request: Request<LockRequest>,
) -> Response<()> {
    let mut temp_file = tempfile::NamedTempFile::new().unwrap();

    let json_string_request = request.to_json_string();

    let _ = temp_file.write(json_string_request.as_bytes()).unwrap();

    let output = Command::new(executable)
        .env("DEMO_CONFIG", config_path)
        .arg("build-tx-info")
        .arg("lock")
        .arg("--request")
        .arg(temp_file.path().to_str().unwrap())
        .output()
        .unwrap();

    let stdout_contents = String::from_utf8(output.stdout.clone()).unwrap();

    if !output.status.success() {
        panic!(
            "{} failed with stderr: {}",
            executable,
            String::from_utf8(output.stderr.clone()).unwrap()
        );
    }

    let result = Json::from_json_string(&stdout_contents).unwrap();

    temp_file.close().unwrap();

    result
}

/// Calls a CLI with the provided executable to produce a `Response` suitable for creating a claim tx
///
/// NOTE(jaredponn) October 21, 2024: the idea is that in the other supported LB languages, there'd
/// be an executable, say `myExecutable` which conforms to the CLI of
/// `myExecutable build-tx-info claim --request <request-filepath>`
/// to build a `Response` suitable for tx-village.
pub fn cli_claim_response(
    executable: &str,
    config_path: &str,
    request: Request<ClaimRequest>,
) -> Response<()> {
    let mut temp_file = tempfile::NamedTempFile::new().unwrap();

    let json_string_request = request.to_json_string();

    let _ = temp_file.write(json_string_request.as_bytes()).unwrap();

    let output = Command::new(executable)
        .env("DEMO_CONFIG", config_path)
        .arg("build-tx-info")
        .arg("claim")
        .arg("--request")
        .arg(temp_file.path().to_str().unwrap())
        .output()
        .unwrap();

    let stdout_contents = String::from_utf8(output.stdout.clone()).unwrap();

    if !output.status.success() {
        panic!(
            "{} failed with stderr: {}",
            executable,
            String::from_utf8(output.stderr.clone()).unwrap()
        );
    }

    let result = Json::from_json_string(&stdout_contents).unwrap();

    temp_file.close().unwrap();

    result
}

/// Calls the demo-tx-village CLI to return the eq_validator address
pub fn demo_tx_village_eq_validator_address(
    config_path: &str,
    network: &tx_bakery::chain_query::Network,
) -> cardano_serialization_lib::address::Address {
    let mut cmd = Command::cargo_bin("demo-tx-village").unwrap();

    let assert = cmd
        .env("DEMO_CONFIG", config_path)
        .arg("addresses")
        .arg("eq-validator")
        .arg("--network")
        .arg(network_to_cli_string(&network));

    let stdout_contents =
        String::from_utf8(assert.assert().success().get_output().stdout.clone()).unwrap();

    cardano_serialization_lib::address::Address::from_bech32(&stdout_contents).unwrap()
}

/// Calls the demo-tx-village CLI to query the blockchain for UTxOs
pub fn demo_tx_village_query_utxos(
    config_path: &str,
    network: &tx_bakery::chain_query::Network,
    addr: cardano_serialization_lib::address::Address,
    option_eq_datum: Option<lbf_demo_plutus_api::demo::plutus::EqDatum>,
) -> std::vec::Vec<plutus_ledger_api::v2::transaction::TxInInfo> {
    let mut cmd = Command::cargo_bin("demo-tx-village").unwrap();

    let mut temp_file = tempfile::NamedTempFile::new().unwrap();

    let mut assert = cmd
        .env("DEMO_CONFIG", config_path)
        .arg("query-utxos")
        .arg("--address")
        .arg(addr.to_bech32(None).unwrap())
        .arg("--network")
        .arg(network_to_cli_string(&network));

    if let Some(eq_datum) = option_eq_datum {
        let json_string_eq_datum = eq_datum.to_json_string();

        temp_file
            .write_all(json_string_eq_datum.as_bytes())
            .unwrap();
        assert = assert
            .arg("--lb-json-eq-datum-filepath")
            .arg(temp_file.path().to_str().unwrap());
    }

    let stdout_contents =
        String::from_utf8(assert.assert().success().get_output().stdout.clone()).unwrap();

    let result = Json::from_json_string(&stdout_contents).unwrap();

    temp_file.close().unwrap();

    result
}

/// Calls the demo-tx-village CLI to build and submit a tx
pub fn demo_tx_village_build_and_submit(
    config_path: &str,
    network: &tx_bakery::chain_query::Network,
    signing_key_file: &str,
    tx_info: plutus_ledger_api::v2::transaction::TransactionInfo,
) -> plutus_ledger_api::v1::transaction::TransactionHash {
    let mut cmd = Command::cargo_bin("demo-tx-village").unwrap();

    let assert = cmd
        .env("DEMO_CONFIG", config_path)
        .arg("build-and-submit")
        .arg("--signing-key-file")
        .arg(signing_key_file)
        .arg("--network")
        .arg(network_to_cli_string(&network))
        .write_stdin(tx_info.to_json_string());

    let assert_success = assert.assert().success();
    let output = assert_success.get_output();

    let stderr_contents = String::from_utf8(output.stderr.clone()).unwrap();

    eprintln!("demo-tx-village stderr:\n```\n{}```", stderr_contents);

    let stdout_contents = String::from_utf8(output.stdout.clone()).unwrap();

    let result = Json::from_json_string(&stdout_contents).unwrap();

    result
}

/// Returns a triplet of:
/// - The first wallet's `.skey` path
/// - The first wallet's `KeyWallet` type
/// - The first wallet's csl `Address`
/// WARNING: this assumes that we used tx-villages plutip launcher which puts the wallets in the
/// `WALLETS_DIR` directory.
pub async fn get_the_wallet(
    network: &tx_bakery::chain_query::Network,
) -> (
    std::path::PathBuf,
    KeyWallet,
    cardano_serialization_lib::address::Address,
) {
    let entries = std::fs::read_dir::<std::path::PathBuf>(WALLETS_DIR.into());

    let an_skey: std::path::PathBuf = entries
        .unwrap()
        .find(|result_dir_entry| match result_dir_entry {
            Ok(dir_entry) => dir_entry.path().extension() == Some(std::ffi::OsStr::new("skey")),
            Err(_) => false,
        })
        .unwrap()
        .expect("Failed to find a secret key file")
        .path();
    let key_wallet = KeyWallet::new(an_skey.clone(), None::<std::path::PathBuf>)
        .await
        .unwrap();
    let key_wallet_change_pkh = key_wallet.get_change_pkh().0 .0;

    (
        an_skey.clone(),
        key_wallet,
        cardano_serialization_lib::address::EnterpriseAddress::new(
            network.to_network_id(),
            &cardano_serialization_lib::address::StakeCredential::from_keyhash(
                &cardano_serialization_lib::crypto::Ed25519KeyHash::from_bytes(
                    key_wallet_change_pkh,
                )
                .unwrap(),
            ),
        )
        .to_address(),
    )
}

/// The hardcoded path of the directory of the wallets
/// NOTE(jaredponn) October 16, 2024: This matches the hardcoded values in tx-village
///
pub static WALLETS_DIR: &str = ".wallets";

/// Creates the test data of
///
/// 1. The datum and redeemer for a sequence of txes which test if the input is equal
///
/// 2. The datum and redeemer for a sequence of txes which test if the input is not equal
///
/// NOTE(jaredponn): October 15,2024. This matches the test data in the CTL implementation
pub fn setup_test_data(
    validator_hash: ValidatorHash,
) -> ((EqDatum, EqRedeemer), (EqDatum, EqRedeemer)) {
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

    let example_eq_redeemer_a = EqRedeemer::IsEqual(example_eq_datum_a.clone());

    let example_eq_datum_b = EqDatum {
        rec: Record {
            bar: example_address.clone(),
            baz: example_plutus_bytes.clone(),
            foo: example_asset_class.clone(),
        },
        sum: Sum::Foo(example_asset_class.clone()),
        prod: Product(example_asset_class, example_address, example_plutus_bytes),
    };

    let example_eq_redeemer_b = EqRedeemer::IsNotEqual(example_eq_datum_a.clone());

    (
        (example_eq_datum_a, example_eq_redeemer_a),
        (example_eq_datum_b, example_eq_redeemer_b),
    )
}

/// Reads scripts at the provided path. This is helpful when used for `setup_test_data`.
pub async fn read_script(path: &str) -> ScriptOrRef {
    let conf_str = fs::read_to_string(path).await.unwrap_or_else(|err| {
        panic!(
            "Couldn't read plutarch config JSON file at {} with error {}.",
            path, err
        )
    });

    let conf: Config = Json::from_json_string(&conf_str).unwrap_or_else(|err| {
        panic!(
            "Couldn't deserialize JSON data of file {} with error {}",
            path, err
        )
    });

    let Script(raw_script) = conf.eq_validator;

    ScriptOrRef::from_bytes(raw_script).unwrap_or_else(|err| {
        panic!(
            "Couldn't deserialize PlutusScript of file {} with error {}",
            path, err
        )
    })
}

/// Sets up plutip + ogmios for the tests
pub async fn setup_plutip_test() -> (Plutip, OgmiosLauncher, OgmiosClient) {
    let plutip_config = PlutipConfigBuilder::default()._utxos(5).build().unwrap();
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

fn network_to_cli_string(network: &tx_bakery::chain_query::Network) -> &str {
    match network {
        tx_bakery::chain_query::Network::Testnet => "testnet",
        tx_bakery::chain_query::Network::Mainnet => "mainnet",
    }
}
