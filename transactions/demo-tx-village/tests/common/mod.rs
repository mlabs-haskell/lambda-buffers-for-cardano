//! Common functionality for writing tests. Namely:
//! - Running an integration test suite
//! - Wrappers for calling the `demo-tx-village` CLI to serialize and deserialize inputs / outputs
//! to a Rust type
//! - Setting up the local devnet along with other dependencies
use assert_cmd::Command;
use lbf_demo_config_api::demo::config::{Config, Script};
use lbf_demo_plutus_api::demo::{
    plutus::{EqDatum, EqRedeemer, Product, Record, Sum},
    request::{ClaimRequest, LockRequest, Request},
    response::Response,
};
use lbr_prelude::json::Json;
use num_bigint::BigInt;
use plutus_ledger_api::v3::{
    address::{Address, Credential},
    crypto::LedgerBytes,
    script::{MintingPolicyHash, ScriptHash, ValidatorHash},
    transaction::{POSIXTime, TransactionHash, TransactionInfo, TxInInfo},
    value::{AssetClass, CurrencySymbol, TokenName},
};
use std::time::SystemTime;
use std::{io::Write, str::FromStr};
use tokio::fs;
use tx_bakery::utils::script::ScriptOrRef;
use tx_bakery::wallet::Wallet;
use tx_bakery::{chain_query::Network, utils::key_wallet::KeyWallet};

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

    let testnet_rt = TestRuntime::setup_testnet().await;
    let network = testnet_rt.network();
    let wallet = testnet_rt.get_own_wallet().await;
    let wallet_addr = wallet.get_change_addr();

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
                change_address: wallet_addr.clone(),
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
                    testnet_rt.own_skey(),
                    result.tx_info,
                )
            }
            lbf_demo_plutus_api::demo::response::Response::Error(err) => {
                panic!("{:?}", err)
            }
        };

        eprintln!("Successfully stored EqDatum A @ EqV with {}", tx_hash);
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
                change_address: wallet.get_change_addr(),
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
                    testnet_rt.own_skey(),
                    result.tx_info,
                )
            }
            lbf_demo_plutus_api::demo::response::Response::Error(err) => {
                panic!("{:?}", err)
            }
        };

        eprintln!(
            "Successfully checked that they are indeed the same in transaction {}",
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
                change_address: wallet.get_change_addr(),
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
                    testnet_rt.own_skey(),
                    result.tx_info,
                )
            }
            lbf_demo_plutus_api::demo::response::Response::Error(err) => {
                panic!("{:?}", err)
            }
        };
        eprintln!("Successfully stored EqDatum B with {}", tx_hash);
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
                change_address: wallet.get_change_addr(),
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
                    testnet_rt.own_skey(),
                    result.tx_info,
                )
            }
            lbf_demo_plutus_api::demo::response::Response::Error(err) => {
                panic!("{:?}", err)
            }
        };

        eprintln!(
            "Successfully check that they are indeed different in transaction {}",
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
pub fn demo_tx_village_eq_validator_address(config_path: &str, network: &Network) -> Address {
    let mut cmd = Command::new("demo-tx-village");

    let assert = cmd
        .env("DEMO_CONFIG", config_path)
        .arg("addresses")
        .arg("eq-validator")
        .arg("--network")
        .arg(network_to_cli_string(network));

    let stdout_contents =
        String::from_utf8(assert.assert().success().get_output().stdout.clone()).unwrap();

    Address::from_str(&stdout_contents).unwrap()
}

/// Calls the demo-tx-village CLI to query the blockchain for UTxOs
pub fn demo_tx_village_query_utxos(
    config_path: &str,
    network: &Network,
    addr: Address,
    option_eq_datum: Option<EqDatum>,
) -> std::vec::Vec<TxInInfo> {
    let mut cmd = Command::new("demo-tx-village");

    let mut temp_file = tempfile::NamedTempFile::new().unwrap();

    let mut assert = cmd
        .env("DEMO_CONFIG", config_path)
        .arg("query-utxos")
        .arg("--address")
        .arg(addr.with_extra_info(network.to_network_id()).to_string())
        .arg("--network")
        .arg(network_to_cli_string(network));

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
    tx_info: TransactionInfo,
) -> TransactionHash {
    let mut cmd = Command::new("demo-tx-village");

    let assert = cmd
        .env("DEMO_CONFIG", config_path)
        .arg("build-and-submit")
        .arg("--signing-key-file")
        .arg(signing_key_file)
        .arg("--network")
        .arg(network_to_cli_string(network))
        .write_stdin(tx_info.to_json_string());

    let assert_success = assert.assert().success();
    let output = assert_success.get_output();

    let stderr_contents = String::from_utf8(output.stderr.clone()).unwrap();

    eprintln!("demo-tx-village stderr:\n```\n{}```", stderr_contents);

    let stdout_contents = String::from_utf8(output.stdout.clone()).unwrap();

    Json::from_json_string(&stdout_contents).unwrap()
}

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
    let example_token_name = TokenName::from_string("example token name").unwrap();
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

    ScriptOrRef::from_bytes_v3(raw_script).unwrap_or_else(|err| {
        panic!(
            "Couldn't deserialize PlutusScript of file {} with error {}",
            path, err
        )
    })
}

struct TestRuntime {
    network: Network,
}

impl TestRuntime {
    async fn setup_testnet() -> Self {
        let network = Network::Testnet;

        TestRuntime { network }
    }

    async fn get_own_wallet(&self) -> KeyWallet {
        KeyWallet::new_enterprise(self.own_skey()).await.unwrap()
    }

    fn own_skey(&self) -> &str {
        "./wallets/test.skey"
    }

    fn network(&self) -> Network {
        self.network.clone()
    }
}

fn network_to_cli_string(network: &Network) -> &str {
    match network {
        Network::Testnet => "testnet",
        Network::Mainnet => "mainnet",
    }
}
