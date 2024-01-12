#[cfg(test)]
mod tests {
    use cardano_serialization_lib::address::{EnterpriseAddress, StakeCredential};
    use cardano_serialization_lib::crypto::{Ed25519KeyHash, TransactionHash};
    use cardano_serialization_lib::plutus::PlutusScript;
    use cardano_serialization_lib::TransactionInput;
    use data_encoding::HEXLOWER;
    use demo_rust::{create_value_tx, input_is_equal_tx, input_is_not_equal_tx};
    use lbf_demo_config_api::demo::config::{Config, Script};
    use lbf_demo_plutus_api::demo::plutus::{EqDatum, Product, Record, Sum};
    use lbr_prelude::json::Json;
    use plutus_ledger_api::v2::address::{Address, Credential};
    use plutus_ledger_api::v2::crypto::LedgerBytes;
    use plutus_ledger_api::v2::script::ValidatorHash;
    use plutus_ledger_api::v2::script::{MintingPolicyHash, ScriptHash};
    use plutus_ledger_api::v2::value::{AssetClass, CurrencySymbol, TokenName};
    use std::collections::BTreeMap;
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

        eq_validator_test(&plutarch_script, &example_eq_datum_a, &example_eq_datum_b);
        eq_validator_test(&plutustx_script, &example_eq_datum_a, &example_eq_datum_b);
    }

    fn read_script(path: &str) -> PlutusScript {
        let conf_str = fs::read_to_string(path).expect(&format!(
            "Couldn't read plutarch config JSON file at {}.",
            path
        ));

        let conf: Config = Json::from_json_string(&conf_str)
            .expect(&format!("Couldn't deserialize JSON data of file {}", path));

        let Script(raw_script) = conf.eq_validator;

        PlutusScript::from_bytes(raw_script).expect(&format!(
            "Couldn't deserialize PlutusScript of file {}.",
            path
        ))
    }

    fn parse_pkh(hex_str: &str) -> Ed25519KeyHash {
        Ed25519KeyHash::from_bytes(HEXLOWER.decode(&hex_str.to_owned().into_bytes()).unwrap())
            .unwrap()
    }

    fn eq_validator_test(
        eq_validator: &PlutusScript,
        example_eq_datum_a: &EqDatum,
        example_eq_datum_b: &EqDatum,
    ) {
        // TODO: Actual network id
        let network_id = 1;
        // TODO: Query own pub key hash
        let own_pkh = parse_pkh("0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546");
        let own_addr = EnterpriseAddress::new(network_id, &StakeCredential::from_keyhash(&own_pkh))
            .to_address();

        // TODO: Query tx inputs to be consumed
        let tx_ins = BTreeMap::new();

        // TODO: Submit this
        let (_create_eq_datum_a_tx_body, eq_datum_a_tx_out) = create_value_tx(
            network_id,
            &own_pkh,
            eq_validator,
            example_eq_datum_a,
            &tx_ins,
        );

        // TODO: Submit this
        let (_create_eq_datum_b_tx_body, eq_datum_b_tx_out) = create_value_tx(
            network_id,
            &own_pkh,
            eq_validator,
            example_eq_datum_b,
            &tx_ins,
        );

        // TODO: This should be the UTxO we just created
        let tx_in_a = TransactionInput::new(
            &TransactionHash::from_hex(
                "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff",
            )
            .unwrap(),
            0,
        );

        // TODO: Query validator UTxOs
        let eq_validator_utxos_a = BTreeMap::from([(&tx_in_a, &eq_datum_a_tx_out)]);

        let _eq_datum_a_is_equal_tx = input_is_equal_tx(
            &own_addr,
            eq_validator,
            eq_validator_utxos_a,
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

        // TODO: Query validator UTxOs
        let eq_validator_utxos_b = BTreeMap::from([(&tx_in_b, &eq_datum_b_tx_out)]);

        let _eq_datum_a_is_equal_tx = input_is_not_equal_tx(
            &own_addr,
            eq_validator,
            eq_validator_utxos_b,
            &tx_in_b,
            example_eq_datum_a,
        );
    }
}
