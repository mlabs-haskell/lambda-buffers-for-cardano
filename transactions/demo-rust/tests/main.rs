mod utils;

#[cfg(test)]
mod tests {
    use super::utils::ogmios::{Ogmios, OgmiosConfigBuilder};
    use super::utils::plutip::{Plutip, PlutipConfigBuilder};
    use super::utils::read_script;
    use cardano_serialization_lib::address::{EnterpriseAddress, StakeCredential};
    use cardano_serialization_lib::crypto::TransactionHash;
    use cardano_serialization_lib::plutus::PlutusScript;
    use cardano_serialization_lib::TransactionInput;
    use demo_rust::{create_value_tx, input_is_equal_tx, input_is_not_equal_tx};
    use lbf_demo_plutus_api::demo::plutus::{EqDatum, Product, Record, Sum};
    use plutus_ledger_api::v2::address::{Address, Credential};
    use plutus_ledger_api::v2::crypto::LedgerBytes;
    use plutus_ledger_api::v2::script::ValidatorHash;
    use plutus_ledger_api::v2::script::{MintingPolicyHash, ScriptHash};
    use plutus_ledger_api::v2::value::{AssetClass, CurrencySymbol, TokenName};
    use std::collections::BTreeMap;

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
        let plutip_config = PlutipConfigBuilder::default().build().unwrap();
        let plutip = Plutip::start(&plutip_config);

        let ogmios_config = OgmiosConfigBuilder::default()
            .node_socket(plutip.get_node_socket())
            .node_config(plutip.get_node_config_path())
            .build()
            .unwrap();
        let ogmios = Ogmios::start(&ogmios_config);

        let network_id = 0b0001;
        let own_pkh = plutip.get_pkh();
        let own_addr = EnterpriseAddress::new(network_id, &StakeCredential::from_keyhash(&own_pkh))
            .to_address();

        let utxos = ogmios.query_utxos(&own_addr).await;

        // TODO: Submit this
        let create_eq_datum_a_tx_body =
            create_value_tx(network_id, eq_validator, example_eq_datum_a, &utxos);

        ogmios
            .evaluate_transaction(&create_eq_datum_a_tx_body.unwrap())
            .await;

        let utxos = ogmios.query_utxos(&own_addr).await;

        // TODO: Submit this
        let _create_eq_datum_b_tx_body =
            create_value_tx(network_id, eq_validator, example_eq_datum_b, &utxos);

        // TODO: This should be the UTxO we just created
        let tx_in_a = TransactionInput::new(
            &TransactionHash::from_hex(
                "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff",
            )
            .unwrap(),
            0,
        );

        let validator_addr = EnterpriseAddress::new(
            network_id,
            &StakeCredential::from_scripthash(&eq_validator.hash()),
        )
        .to_address();

        let eq_validator_utxos_a = ogmios.query_utxos(&validator_addr).await;
        let utxos = ogmios.query_utxos(&own_addr).await;

        let _eq_datum_a_is_equal_tx = input_is_equal_tx(
            &own_addr,
            &utxos,
            eq_validator,
            &eq_validator_utxos_a,
            &tx_in_a,
            example_eq_datum_a,
        );

        // TODO: This should be the UTxO we just created
        let tx_in_b = TransactionInput::new(
            &TransactionHash::from_hex(
                "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff",
            )
            .unwrap(),
            0,
        );

        let eq_validator_utxos_b = ogmios.query_utxos(&validator_addr).await;
        let utxos = ogmios.query_utxos(&own_addr).await;

        let _eq_datum_a_is_equal_tx = input_is_not_equal_tx(
            &own_addr,
            &utxos,
            eq_validator,
            &eq_validator_utxos_b,
            &tx_in_b,
            example_eq_datum_a,
        );
    }
}
