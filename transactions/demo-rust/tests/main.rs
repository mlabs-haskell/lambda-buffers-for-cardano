mod utils;

#[cfg(test)]
mod tests {
    use super::utils::plutip::{Plutip, PlutipConfigBuilder};
    use cardano_serialization_lib::plutus::PlutusScript;
    use cardano_serialization_lib::TransactionInput;
    use demo_rust::utils::ogmios::{Ogmios, OgmiosConfigBuilder};
    use demo_rust::utils::wallet::Wallet;
    use demo_rust::{claim_tx_build_and_submit, lock_tx_build_and_submit};
    use lbf_demo_config_api::demo::config::{Config, Script};
    use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer, Product, Record, Sum};
    use lbr_prelude::json::Json;
    use plutus_ledger_api::v2::address::{Address, Credential};
    use plutus_ledger_api::v2::crypto::LedgerBytes;
    use plutus_ledger_api::v2::script::ValidatorHash;
    use plutus_ledger_api::v2::script::{MintingPolicyHash, ScriptHash};
    use plutus_ledger_api::v2::value::{AssetClass, CurrencySymbol, TokenName};
    use std::fs;

    #[test]
    fn test_it() {
        let plutarch_script = read_script("data/demo-plutarch-config.json");
        let plutustx_script = read_script("data/demo-plutustx-config.json");

        let example_token_name = TokenName(LedgerBytes(b"example token name".to_vec()));
        let example_currency_symbol =
            CurrencySymbol::NativeToken(MintingPolicyHash(ScriptHash(LedgerBytes(b"".to_vec()))));

        let example_asset_class = AssetClass {
            currency_symbol: example_currency_symbol,
            token_name: example_token_name,
        };
        let example_plutus_bytes = LedgerBytes(b"example bytes".to_vec());

        let example_address = Address {
            credential: Credential::Script(ValidatorHash(ScriptHash(LedgerBytes(
                plutarch_script.hash().to_bytes(),
            )))),
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

        tokio_test::block_on(eq_validator_test(
            &plutarch_script,
            &example_eq_datum_a,
            &example_eq_datum_b,
        ));
        tokio_test::block_on(eq_validator_test(
            &plutustx_script,
            &example_eq_datum_a,
            &example_eq_datum_b,
        ));
    }

    async fn eq_validator_test(
        eq_validator: &PlutusScript,
        example_eq_datum_a: &EqDatum,
        example_eq_datum_b: &EqDatum,
    ) {
        let (plutip, ogmios) = setup_plutip_test().await;

        let tx_hash_lock_a =
            lock_tx_build_and_submit(&plutip, &ogmios, eq_validator, example_eq_datum_a).await;

        ogmios.await_tx_confirm(&tx_hash_lock_a).await;

        // let tx_hash_lock_b =
        //     lock_tx_build_and_submit(&plutip, &ogmios, eq_validator, example_eq_datum_b).await;

        // ogmios.await_tx_confirm(&tx_hash_lock_b).await;

        // TODO: Verify that the tx index is correct (includes datum)
        let tx_in_a = TransactionInput::new(&tx_hash_lock_a, 0);

        let tx_hash_claim_a = claim_tx_build_and_submit(
            &plutip,
            &ogmios,
            eq_validator,
            &EqRedeemer::IsEqual(example_eq_datum_a.clone()),
            &tx_in_a,
        )
        .await;

        ogmios.await_tx_confirm(&tx_hash_claim_a).await;

        // // TODO: Verify that the tx index is correct (includes datum)
        // let tx_in_b = TransactionInput::new(&tx_hash_lock_b, 0);

        // let tx_hash_claim_b = claim_tx_build_and_submit(
        //     &plutip,
        //     &ogmios,
        //     eq_validator,
        //     &EqRedeemer::IsNotEqual(example_eq_datum_a.clone()),
        //     &tx_in_b,
        // )
        // .await;
        //
        // ogmios.await_tx_confirm(&tx_hash_claim_b).await;
    }

    async fn setup_plutip_test() -> (impl Wallet, Ogmios) {
        let plutip_config = PlutipConfigBuilder::default().build().unwrap();
        let plutip = Plutip::start(&plutip_config).await;

        let ogmios_config = OgmiosConfigBuilder::default()
            .node_socket(plutip.get_node_socket())
            .node_config(plutip.get_node_config_path())
            .build()
            .unwrap();
        let ogmios = Ogmios::start(&ogmios_config).await;

        (plutip, ogmios)
    }

    fn read_script(path: &str) -> PlutusScript {
        let conf_str = fs::read_to_string(path).expect(&format!(
            "Couldn't read plutarch config JSON file at {}.",
            path
        ));

        let conf: Config = Json::from_json_string(&conf_str)
            .expect(&format!("Couldn't deserialize JSON data of file {}", path));

        let Script(raw_script) = conf.eq_validator;

        let mut serializer = cbor_event::se::Serializer::new_vec();
        serializer.write_bytes(raw_script).unwrap();
        let script = serializer.finalize();

        PlutusScript::from_bytes_v2(script).expect(&format!(
            "Couldn't deserialize PlutusScript of file {}.",
            path
        ))
    }
}
