#[cfg(test)]
mod tests {
    use demo_rust::{claim_eq_datum, lock_eq_datum};
    use lbf_demo_config_api::demo::config::{Config, Script};
    use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer, Product, Record, Sum};
    use lbr_prelude::json::Json;
    use plutus_ledger_api::v2::address::{Address, Credential};
    use plutus_ledger_api::v2::crypto::LedgerBytes;
    use plutus_ledger_api::v2::script::ValidatorHash;
    use plutus_ledger_api::v2::script::{MintingPolicyHash, ScriptHash};
    use plutus_ledger_api::v2::value::{AssetClass, CurrencySymbol, TokenName};
    use serial_test::serial;
    use std::fs;
    use tx_bakery::submitter::Submitter;
    use tx_bakery::utils::ogmios::client::{OgmiosClient, OgmiosClientConfigBuilder};
    use tx_bakery::utils::ogmios::launcher::{OgmiosLauncher, OgmiosLauncherConfigBuilder};
    use tx_bakery::utils::plutip::{Plutip, PlutipConfigBuilder};
    use tx_bakery::utils::script::ScriptOrRef;
    use url::Url;

    #[tokio::test]
    #[serial]
    async fn test_plutarch_is_equal() {
        let plutarch_script = read_script("data/demo-plutarch-config.json").as_validator();
        let (example_eq_datum_a, _) = setup_test_data();

        is_eq_validator_test(&plutarch_script, &example_eq_datum_a).await
    }

    #[tokio::test]
    #[serial]
    async fn test_plutarch_is_not_equal() {
        let plutarch_script = read_script("data/demo-plutarch-config.json").as_validator();
        let (example_eq_datum_a, example_eq_datum_b) = setup_test_data();

        is_not_eq_validator_test(&plutarch_script, &example_eq_datum_a, &example_eq_datum_b).await
    }

    #[tokio::test]
    #[serial]
    async fn test_plutustx_is_equal() {
        let plutustx_script = read_script("data/demo-plutustx-config.json").as_validator();
        let (example_eq_datum_a, _) = setup_test_data();

        is_eq_validator_test(&plutustx_script, &example_eq_datum_a).await
    }

    #[tokio::test]
    #[serial]
    async fn test_plutustx_is_not_equal() {
        let plutustx_script = read_script("data/demo-plutustx-config.json").as_validator();
        let (example_eq_datum_a, example_eq_datum_b) = setup_test_data();

        is_not_eq_validator_test(&plutustx_script, &example_eq_datum_a, &example_eq_datum_b).await
    }

    async fn is_eq_validator_test(
        eq_validator: &(ValidatorHash, ScriptOrRef),
        example_eq_datum: &EqDatum,
    ) {
        let (plutip, _ogmios_launcher, ogmios) = setup_plutip_test().await;

        let tx_hash_lock_a = lock_eq_datum::build_and_submit(
            &plutip.get_own_wallet().await.unwrap(),
            &ogmios,
            &ogmios,
            eq_validator.clone(),
            example_eq_datum,
        )
        .await
        .unwrap();

        ogmios.await_tx_confirm(&tx_hash_lock_a).await.unwrap();

        let tx_hash_claim_a = claim_eq_datum::build_and_submit(
            &plutip.get_own_wallet().await.unwrap(),
            &ogmios,
            &ogmios,
            eq_validator.clone(),
            &EqRedeemer::IsEqual(example_eq_datum.clone()),
            &example_eq_datum,
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
        let (plutip, _ogmios_launcher, ogmios) = setup_plutip_test().await;

        let tx_hash_lock_b = lock_eq_datum::build_and_submit(
            &plutip.get_own_wallet().await.unwrap(),
            &ogmios,
            &ogmios,
            eq_validator.clone(),
            example_eq_datum_b,
        )
        .await
        .unwrap();

        ogmios.await_tx_confirm(&tx_hash_lock_b).await.unwrap();

        let tx_hash_claim_b = claim_eq_datum::build_and_submit(
            &plutip.get_own_wallet().await.unwrap(),
            &ogmios,
            &ogmios,
            eq_validator.clone(),
            &EqRedeemer::IsNotEqual(example_eq_datum_a.clone()),
            &example_eq_datum_b,
        )
        .await
        .unwrap();

        ogmios.await_tx_confirm(&tx_hash_claim_b).await.unwrap()
    }

    async fn setup_plutip_test() -> (Plutip, OgmiosLauncher, OgmiosClient) {
        let plutip_config = PlutipConfigBuilder::default().build().unwrap();
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

    fn setup_test_data() -> (EqDatum, EqDatum) {
        let plutarch_script = read_script("data/demo-plutarch-config.json");

        let example_token_name = TokenName::from_string("example token name");
        let example_currency_symbol =
            CurrencySymbol::NativeToken(MintingPolicyHash(ScriptHash(LedgerBytes([0].repeat(28)))));

        let example_asset_class = AssetClass {
            currency_symbol: example_currency_symbol,
            token_name: example_token_name,
        };
        let example_plutus_bytes = LedgerBytes(b"example bytes".to_vec());

        let example_address = Address {
            credential: Credential::Script(ValidatorHash(plutarch_script.get_script_hash())),
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

    fn read_script(path: &str) -> ScriptOrRef {
        let conf_str = fs::read_to_string(path).expect(&format!(
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
}
