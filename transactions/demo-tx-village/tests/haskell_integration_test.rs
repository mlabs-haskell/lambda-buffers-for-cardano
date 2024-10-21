mod common;

use serial_test::serial;

/// Runs the integration test with the plutarch scripts with the `demo-haskell` CLI
#[tokio::test]
#[serial]
async fn haskell_plutarch_integration_test() {
    let executable = "demo-haskell";
    let config_path = "data/demo-plutarch-config.json";
    common::run_integration_test(executable, config_path).await;
}

/// Runs the integration test with the PlutusTx scripts with the `demo-haskell` CLI
#[tokio::test]
#[serial]
async fn haskell_plutustx_integration_test() {
    let executable = "demo-haskell";
    let config_path = "data/demo-plutustx-config.json";
    common::run_integration_test(executable, config_path).await;
}
