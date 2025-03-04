#[cfg(test)]
mod tests {
    use demo_rust::{claim_eq_datum, lock_eq_datum};
    use lbf_demo_config_api::demo::config::{Config, Script};
    use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer, Product, Record, Sum};
    use lbr_prelude::json::Json;
    use plutus_ledger_api::v3::{
        address::{Address, Credential},
        crypto::LedgerBytes,
        script::ValidatorHash,
        script::{MintingPolicyHash, ScriptHash},
        value::{AssetClass, CurrencySymbol, TokenName},
    };
    use serial_test::serial;
    use tokio::fs;
    use tx_bakery::{
        chain_query::Network,
        submitter::Submitter,
        utils::{key_wallet::KeyWallet, script::ScriptOrRef},
    };
    use tx_bakery_ogmios::client::{OgmiosClient, OgmiosClientConfigBuilder};
    use url::Url;

    struct TestRuntime {
        ogmios_client: OgmiosClient,
    }

    impl TestRuntime {
        async fn setup_testnet() -> Self {
            let ogmios_client_config = OgmiosClientConfigBuilder::default()
                .network(Network::Testnet)
                .url(Url::parse("http://127.0.0.1:1337").unwrap())
                .build()
                .unwrap();
            let ogmios_client = OgmiosClient::connect(ogmios_client_config).await.unwrap();

            TestRuntime { ogmios_client }
        }

        async fn get_own_wallet(&self) -> KeyWallet {
            KeyWallet::new_enterprise("./wallets/test.skey")
                .await
                .unwrap()
        }

        fn ogmios_client(&self) -> &OgmiosClient {
            &self.ogmios_client
        }
    }

    #[tokio::test]
    #[serial]
    async fn test_plutarch_is_equal() {
        let plutarch_script = read_script("data/demo-plutarch-config.json")
            .await
            .as_validator();
        let (example_eq_datum_a, _) = setup_test_data(plutarch_script.0.clone());

        is_eq_validator_test(&plutarch_script, &example_eq_datum_a).await
    }

    #[tokio::test]
    #[serial]
    async fn test_plutarch_is_not_equal() {
        let plutarch_script = read_script("data/demo-plutarch-config.json")
            .await
            .as_validator();
        let (example_eq_datum_a, example_eq_datum_b) = setup_test_data(plutarch_script.0.clone());

        is_not_eq_validator_test(&plutarch_script, &example_eq_datum_a, &example_eq_datum_b).await
    }

    #[tokio::test]
    #[serial]
    async fn test_plutustx_is_equal() {
        let plutustx_script = read_script("data/demo-plutustx-config.json")
            .await
            .as_validator();
        let (example_eq_datum_a, _) = setup_test_data(plutustx_script.0.clone());

        is_eq_validator_test(&plutustx_script, &example_eq_datum_a).await
    }

    #[tokio::test]
    #[serial]
    async fn test_plutustx_is_not_equal() {
        let plutustx_script = read_script("data/demo-plutustx-config.json")
            .await
            .as_validator();
        let (example_eq_datum_a, example_eq_datum_b) = setup_test_data(plutustx_script.0.clone());

        is_not_eq_validator_test(&plutustx_script, &example_eq_datum_a, &example_eq_datum_b).await
    }

    async fn is_eq_validator_test(
        eq_validator: &(ValidatorHash, ScriptOrRef),
        example_eq_datum: &EqDatum,
    ) {
        let test_runtime = TestRuntime::setup_testnet().await;
        let ogmios = test_runtime.ogmios_client();

        let tx_hash_lock_a = lock_eq_datum::build_and_submit(
            &test_runtime.get_own_wallet().await,
            ogmios,
            ogmios,
            eq_validator.clone(),
            example_eq_datum,
        )
        .await
        .unwrap();

        ogmios.await_tx_confirm(&tx_hash_lock_a).await.unwrap();

        let tx_hash_claim_a = claim_eq_datum::build_and_submit(
            &test_runtime.get_own_wallet().await,
            ogmios,
            ogmios,
            eq_validator.clone(),
            &EqRedeemer::IsEqual(example_eq_datum.clone()),
            example_eq_datum,
        )
        .await
        .unwrap();

        ogmios.await_tx_confirm(&tx_hash_claim_a).await.unwrap();
    }

    async fn is_not_eq_validator_test(
        eq_validator: &(ValidatorHash, ScriptOrRef),
        example_eq_datum_a: &EqDatum,
        example_eq_datum_b: &EqDatum,
    ) {
        let test_runtime = TestRuntime::setup_testnet().await;
        let ogmios = test_runtime.ogmios_client();

        let tx_hash_lock_b = lock_eq_datum::build_and_submit(
            &test_runtime.get_own_wallet().await,
            ogmios,
            ogmios,
            eq_validator.clone(),
            example_eq_datum_b,
        )
        .await
        .unwrap();

        ogmios.await_tx_confirm(&tx_hash_lock_b).await.unwrap();

        let tx_hash_claim_b = claim_eq_datum::build_and_submit(
            &test_runtime.get_own_wallet().await,
            ogmios,
            ogmios,
            eq_validator.clone(),
            &EqRedeemer::IsNotEqual(example_eq_datum_a.clone()),
            example_eq_datum_b,
        )
        .await
        .unwrap();

        ogmios.await_tx_confirm(&tx_hash_claim_b).await.unwrap()
    }

    fn setup_test_data(validator_hash: ValidatorHash) -> (EqDatum, EqDatum) {
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

        let example_eq_datum_b = EqDatum {
            rec: Record {
                bar: example_address.clone(),
                baz: example_plutus_bytes.clone(),
                foo: example_asset_class.clone(),
            },
            sum: Sum::Foo(example_asset_class.clone()),
            prod: Product(example_asset_class, example_address, example_plutus_bytes),
        };
        (example_eq_datum_a, example_eq_datum_b)
    }

    async fn read_script(path: &str) -> ScriptOrRef {
        let conf_str = fs::read_to_string(path).await.expect(&format!(
            "Couldn't read plutarch config JSON file at {}.",
            path
        ));

        let conf: Config = Json::from_json_string(&conf_str)
            .expect(&format!("Couldn't deserialize JSON data of file {}", path));

        let Script(raw_script) = conf.eq_validator;

        ScriptOrRef::from_bytes_v3(raw_script).expect(&format!(
            "Couldn't deserialize PlutusScript of file {}.",
            path
        ))
    }
}
