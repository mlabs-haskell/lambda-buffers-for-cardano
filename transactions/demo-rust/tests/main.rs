mod utils;

#[cfg(test)]
mod tests {
    use super::utils::ogmios::{Ogmios, OgmiosConfigBuilder};
    use super::utils::plutip::{Plutip, PlutipConfigBuilder};
    use super::utils::read_script;
    use cardano_serialization_lib::address::{EnterpriseAddress, StakeCredential};
    use cardano_serialization_lib::crypto::TransactionHash;
    use cardano_serialization_lib::plutus::{ExUnits, PlutusScript, Redeemer, RedeemerTag};
    use cardano_serialization_lib::utils::to_bignum;
    use cardano_serialization_lib::TransactionInput;
    use demo_rust::utils::convert_plutus_data;
    use demo_rust::{create_value_tx, input_is_equal_tx, input_is_not_equal_tx};
    use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer, Product, Record, Sum};
    use plutus_ledger_api::plutus_data::IsPlutusData;
    use plutus_ledger_api::v2::address::{Address, Credential};
    use plutus_ledger_api::v2::crypto::LedgerBytes;
    use plutus_ledger_api::v2::script::ValidatorHash;
    use plutus_ledger_api::v2::script::{MintingPolicyHash, ScriptHash};
    use plutus_ledger_api::v2::value::{AssetClass, CurrencySymbol, TokenName};

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

        let tx_hash_a =
            create_value_build_and_submit(&plutip, &ogmios, eq_validator, example_eq_datum_a).await;

        // TODO await confirmation
        tokio::time::sleep(std::time::Duration::from_secs(30)).await;

        let tx_hash_b =
            create_value_build_and_submit(&plutip, &ogmios, eq_validator, example_eq_datum_b).await;

        // TODO await confirmation
        tokio::time::sleep(std::time::Duration::from_secs(30)).await;

        // TODO: This should be the UTxO we just created
        let tx_in_a = TransactionInput::new(&tx_hash_a, 0);

        is_eq_build_and_submit(&plutip, &ogmios, eq_validator, example_eq_datum_a, &tx_in_a).await;

        // TODO await confirmation
        tokio::time::sleep(std::time::Duration::from_secs(30)).await;

        // TODO: This should be the UTxO we just created
        let tx_in_b = TransactionInput::new(&tx_hash_b, 0);

        is_not_eq_build_and_submit(&plutip, &ogmios, eq_validator, example_eq_datum_a, &tx_in_b)
            .await;
    }

    async fn create_value_build_and_submit(
        plutip: &Plutip,
        ogmios: &Ogmios,
        eq_validator: &PlutusScript,
        example_eq_datum: &EqDatum,
    ) -> TransactionHash {
        let utxos = ogmios.query_utxos(&plutip.get_own_addr()).await;

        let eq_validator_addr = EnterpriseAddress::new(
            plutip.get_network_id(),
            &StakeCredential::from_scripthash(&eq_validator.hash()),
        )
        .to_address();

        let create_eq_datum_a_tx_builder = create_value_tx(
            &plutip.get_own_pkh(),
            &eq_validator_addr,
            example_eq_datum,
            &utxos,
        );

        let costmdls = ogmios.query_costmdls().await;

        ogmios
            .balance_sign_and_submit_transacton(
                create_eq_datum_a_tx_builder,
                &plutip.get_priv_key(),
                &plutip.get_own_addr(),
                Vec::new(),
                Vec::new(),
                &costmdls,
            )
            .await
    }

    async fn is_eq_build_and_submit(
        plutip: &Plutip,
        ogmios: &Ogmios,
        eq_validator: &PlutusScript,
        example_eq_datum: &EqDatum,
        tx_in: &TransactionInput,
    ) -> TransactionHash {
        let validator_addr = EnterpriseAddress::new(
            plutip.get_network_id(),
            &StakeCredential::from_scripthash(&eq_validator.hash()),
        )
        .to_address();

        let eq_validator_utxos_a = ogmios.query_utxos(&validator_addr).await;
        let utxos = ogmios.query_utxos(&plutip.get_own_addr()).await;

        let collateral = utxos.keys().next().unwrap();

        let eq_datum_a_is_equal_tx = input_is_equal_tx(
            &plutip.get_own_pkh(),
            &plutip.get_own_addr(),
            &utxos,
            eq_validator,
            &eq_validator_utxos_a,
            &tx_in,
            &collateral,
            example_eq_datum,
        );

        let redeemer_data =
            convert_plutus_data(EqRedeemer::IsEqual(example_eq_datum.to_owned()).to_plutus_data());
        let evaled = ogmios
            .evaluate_transaction(
                &eq_datum_a_is_equal_tx,
                vec![eq_validator],
                vec![&redeemer_data],
            )
            .await;

        let redeemer = Redeemer::new(
            &RedeemerTag::new_spend(),
            &to_bignum(0),
            &redeemer_data,
            &evaled,
        );

        let costmdls = ogmios.query_costmdls().await;

        ogmios
            .balance_sign_and_submit_transacton(
                eq_datum_a_is_equal_tx,
                &plutip.get_priv_key(),
                &plutip.get_own_addr(),
                vec![eq_validator],
                vec![&redeemer],
                &costmdls,
            )
            .await
    }

    async fn is_not_eq_build_and_submit(
        plutip: &Plutip,
        ogmios: &Ogmios,
        eq_validator: &PlutusScript,
        example_eq_datum: &EqDatum,
        tx_in: &TransactionInput,
    ) -> TransactionHash {
        let validator_addr = EnterpriseAddress::new(
            plutip.get_network_id(),
            &StakeCredential::from_scripthash(&eq_validator.hash()),
        )
        .to_address();

        let eq_validator_utxos_b = ogmios.query_utxos(&validator_addr).await;
        let utxos = ogmios.query_utxos(&plutip.get_own_addr()).await;

        let collateral = utxos.keys().next().unwrap();

        let eq_datum_b_is_not_equal_tx = input_is_not_equal_tx(
            &plutip.get_own_pkh(),
            &plutip.get_own_addr(),
            &utxos,
            eq_validator,
            &eq_validator_utxos_b,
            &tx_in,
            &collateral,
            example_eq_datum,
        );

        let redeemer = Redeemer::new(
            &RedeemerTag::new_spend(),
            &to_bignum(0),
            &convert_plutus_data(
                EqRedeemer::IsNotEqual(example_eq_datum.to_owned()).to_plutus_data(),
            ),
            &ExUnits::new(&to_bignum(1), &to_bignum(2)),
        );

        let costmdls = ogmios.query_costmdls().await;

        ogmios
            .balance_sign_and_submit_transacton(
                eq_datum_b_is_not_equal_tx,
                &plutip.get_priv_key(),
                &plutip.get_own_addr(),
                vec![eq_validator],
                vec![&redeemer],
                &costmdls,
            )
            .await
    }

    async fn setup_plutip_test() -> (Plutip, Ogmios) {
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
}
