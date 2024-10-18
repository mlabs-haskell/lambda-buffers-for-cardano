mod common;

use lbf_demo_plutus_api::demo::request::{ClaimRequest, LockRequest, Request};
use lbf_demo_plutus_api::demo::response::Response;
use num_bigint::BigInt;
use plutus_ledger_api::json::Json;
use plutus_ledger_api::plutus_data::IsPlutusData;
use plutus_ledger_api::v1::transaction::POSIXTime;
use serial_test::serial;
use std::io::Write;
use std::process::Command;
use std::time::SystemTime;
use tx_bakery::wallet::Wallet;

/// Calls the `demo-haskell` CLI to produce a `Response` suitable for creating a lock tx
fn demo_haskell_lock_response(config_path: &str, request: Request<LockRequest>) -> Response<()> {
    let mut temp_file = tempfile::NamedTempFile::new().unwrap();

    let json_string_request = request.to_json_string();

    let _ = temp_file.write(json_string_request.as_bytes()).unwrap();

    let output = Command::new("demo-haskell")
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
            "demo-haskell failed with stderr: {}",
            String::from_utf8(output.stderr.clone()).unwrap()
        );
    }

    let result = Json::from_json_string(&stdout_contents).unwrap();

    temp_file.close().unwrap();

    result
}

/// Calls the `demo-haskell` CLI to produce a `Response` suitable for creating a claim tx
fn demo_haskell_claim_response(config_path: &str, request: Request<ClaimRequest>) -> Response<()> {
    let mut temp_file = tempfile::NamedTempFile::new().unwrap();

    let json_string_request = request.to_json_string();

    let _ = temp_file.write(json_string_request.as_bytes()).unwrap();

    let output = Command::new("demo-haskell")
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
            "demo-haskell failed with stderr: {}",
            String::from_utf8(output.stderr.clone()).unwrap()
        );
    }

    let result = Json::from_json_string(&stdout_contents).unwrap();

    temp_file.close().unwrap();

    result
}

/// Runs an integration test with Haskell with the provided config path
async fn run_haskell_integration_test(config_path: &str) {
    let script = common::read_script(config_path).await.as_validator();

    let ((example_eq_datum_a, example_eq_redeemer_a), (example_eq_datum_b, example_eq_redeemer_b)) =
        common::setup_test_data(script.0.clone());

    let (plutip, _ogmios_launcher, _ogmios) = common::setup_plutip_test().await;

    let network = plutip.get_network();

    let (skey_path, key_wallet, wallet_addr) = common::get_the_wallet(&network).await;

    // Create the UTxO for the lock command (storing the datum)
    // -------------------------------
    {
        eprintln!("Storing EqDatum A @ EqV");
        let change_utxos = common::query_utxos(config_path, &network, wallet_addr.clone(), None);

        let current_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_millis();

        let lock_response = demo_haskell_lock_response(
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
                common::build_and_submit(
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

        let change_utxos = common::query_utxos(config_path, &network, wallet_addr.clone(), None);

        let eq_validator_addr = common::eq_validator_address(config_path, &network);

        let utxos_for_datum = common::query_utxos(
            config_path,
            &network,
            eq_validator_addr.clone(),
            Some(example_eq_datum_a.to_plutus_data()),
        );

        let utxo_for_datum = &utxos_for_datum[0];

        let current_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_millis();

        let claim_response = demo_haskell_claim_response(
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
                common::build_and_submit(
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
        let change_utxos = common::query_utxos(config_path, &network, wallet_addr.clone(), None);

        let current_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_millis();

        let lock_response = demo_haskell_lock_response(
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
                common::build_and_submit(
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

        let change_utxos = common::query_utxos(config_path, &network, wallet_addr.clone(), None);

        let eq_validator_addr = common::eq_validator_address(config_path, &network);

        let utxos_for_datum = common::query_utxos(
            config_path,
            &network,
            eq_validator_addr.clone(),
            Some(example_eq_datum_b.to_plutus_data()),
        );

        let utxo_for_datum = &utxos_for_datum[0];

        let current_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_millis();

        let claim_response = demo_haskell_claim_response(
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
                common::build_and_submit(
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

#[tokio::test]
#[serial]
async fn haskell_plutarch_integration_test() {
    let config_path = "data/demo-plutarch-config.json";
    run_haskell_integration_test(config_path).await;
}

#[tokio::test]
#[serial]
async fn haskell_plutustx_integration_test() {
    let config_path = "data/demo-plutustx-config.json";
    run_haskell_integration_test(config_path).await;
}
