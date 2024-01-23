use crate::utils::ogmios::Ogmios;
use crate::utils::wallet::Wallet;
use crate::utils::{convert_plutus_data, create_tx_builder, to_redeemer};
use cardano_serialization_lib as csl;
use cardano_serialization_lib::address::{EnterpriseAddress, StakeCredential};
use cardano_serialization_lib::crypto::Ed25519KeyHash;
use cardano_serialization_lib::crypto::TransactionHash;
use cardano_serialization_lib::output_builder::TransactionOutputBuilder;
use cardano_serialization_lib::plutus::PlutusScript;
use cardano_serialization_lib::tx_builder::tx_inputs_builder::{PlutusWitness, TxInputsBuilder};
use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer};
use plutus_ledger_api::plutus_data::IsPlutusData;
use std::collections::BTreeMap;

pub mod utils;

const COINS_PER_UTXO_WORD: u64 = 34_482;

/// Transaction that stores a EqDatum value at the Eq Validator.
pub fn mk_lock_tx(
    own_pkh: &Ed25519KeyHash,
    validator_addr: &csl::address::Address,
    eq_datum: &EqDatum,
    own_utxos: &BTreeMap<csl::TransactionInput, csl::TransactionOutput>,
) -> csl::tx_builder::TransactionBuilder {
    let mut tx_builder = create_tx_builder();
    let datum = convert_plutus_data(eq_datum.to_plutus_data());
    let data_cost = csl::DataCost::new_coins_per_byte(&csl::utils::to_bignum(COINS_PER_UTXO_WORD));

    let mut available_inputs = csl::utils::TransactionUnspentOutputs::new();
    own_utxos.iter().for_each(|(tx_in, tx_out)| {
        let utxo = csl::utils::TransactionUnspentOutput::new(&tx_in, &tx_out);
        available_inputs.add(&utxo);
    });

    let tx_out = TransactionOutputBuilder::new()
        .with_address(&validator_addr)
        .with_plutus_data(&datum)
        .next()
        .unwrap()
        .with_asset_and_min_required_coin_by_utxo_cost(&csl::MultiAsset::new(), &data_cost)
        .unwrap()
        .build()
        .unwrap();

    tx_builder.add_output(&tx_out).unwrap();
    tx_builder
        .add_inputs_from(
            &available_inputs,
            csl::tx_builder::CoinSelectionStrategyCIP2::RandomImproveMultiAsset,
        )
        .unwrap();
    tx_builder.add_required_signer(own_pkh);

    tx_builder
}

// Make a transaction that releases UTxO stored at the `EqValidator` in one of the cases below:
// - redeemer is `IsEqual` and the supplied plutus data is equal to the one locked as datum
// - redeemer is `IsNotEqual` and the supplied plutus data is not equal to the one locked as datum
pub fn mk_claim_tx(
    own_pkh: &Ed25519KeyHash,
    own_addr: &csl::address::Address,
    own_utxos: &BTreeMap<csl::TransactionInput, csl::TransactionOutput>,
    eq_validator: &csl::plutus::PlutusScript,
    eq_validator_utxos: &BTreeMap<csl::TransactionInput, csl::TransactionOutput>,
    tx_input: &csl::TransactionInput,
    collateral: &csl::TransactionInput,
    eq_redeemer: &EqRedeemer,
) -> csl::tx_builder::TransactionBuilder {
    let mut tx_builder = create_tx_builder();
    let tx_input_resolved = eq_validator_utxos.get(tx_input).unwrap();
    let value = tx_input_resolved.amount();
    let datum = tx_input_resolved
        .plutus_data()
        .expect("Expected an inline datum in transaction input.");

    let redeemer = to_redeemer(&convert_plutus_data(eq_redeemer.to_plutus_data()));
    let mut tx_inputs_builder = TxInputsBuilder::new();
    let data_cost = csl::DataCost::new_coins_per_byte(&csl::utils::to_bignum(COINS_PER_UTXO_WORD));

    let tx_input_witness = PlutusWitness::new(eq_validator, &datum, &redeemer);
    tx_inputs_builder.add_plutus_script_input(&tx_input_witness, tx_input, &value);

    let mut available_inputs = csl::utils::TransactionUnspentOutputs::new();
    own_utxos.iter().for_each(|(tx_in, tx_out)| {
        let utxo = csl::utils::TransactionUnspentOutput::new(&tx_in, &tx_out);
        available_inputs.add(&utxo);
    });

    let mut collateral_builder = TxInputsBuilder::new();
    let collateral_amount = own_utxos.get(&collateral).unwrap().amount();
    collateral_builder.add_input(&own_addr, &collateral, &collateral_amount);

    tx_builder
        .add_output(
            &TransactionOutputBuilder::new()
                .with_address(&own_addr)
                .next()
                .unwrap()
                .with_asset_and_min_required_coin_by_utxo_cost(&csl::MultiAsset::new(), &data_cost)
                .unwrap()
                .build()
                .unwrap(),
        )
        .unwrap();

    tx_builder.set_inputs(&tx_inputs_builder);
    tx_builder.set_collateral(&collateral_builder);
    tx_builder
        .add_inputs_from(
            &available_inputs,
            csl::tx_builder::CoinSelectionStrategyCIP2::RandomImproveMultiAsset,
        )
        .unwrap();
    tx_builder.add_required_signer(own_pkh);

    tx_builder
}

pub async fn lock_tx_build_and_submit(
    wallet: &impl Wallet,
    ogmios: &Ogmios,
    eq_validator: &PlutusScript,
    example_eq_datum: &EqDatum,
) -> TransactionHash {
    let utxos = ogmios.query_utxos(&wallet.get_own_addr()).await;

    let eq_validator_addr = EnterpriseAddress::new(
        wallet.get_network_id(),
        &StakeCredential::from_scripthash(&eq_validator.hash()),
    )
    .to_address();

    let create_eq_datum_a_tx_builder = mk_lock_tx(
        &wallet.get_own_pkh(),
        &eq_validator_addr,
        example_eq_datum,
        &utxos,
    );

    ogmios
        .balance_sign_and_submit_transacton(
            create_eq_datum_a_tx_builder,
            wallet,
            &wallet.get_own_addr(),
            &Vec::new(),
            &Vec::new(),
            &Vec::new(),
        )
        .await
}

pub async fn claim_tx_build_and_submit(
    wallet: &impl Wallet,
    ogmios: &Ogmios,
    eq_validator: &PlutusScript,
    eq_redeemer: &EqRedeemer,
    datum: &EqDatum,
) -> TransactionHash {
    let validator_addr = EnterpriseAddress::new(
        wallet.get_network_id(),
        &StakeCredential::from_scripthash(&eq_validator.hash()),
    )
    .to_address();

    let eq_validator_utxos = ogmios.query_utxos(&validator_addr).await;
    let utxos = ogmios.query_utxos(&wallet.get_own_addr()).await;

    let (tx_in, _) = eq_validator_utxos
        .iter()
        .find(|(_, tx_out)| {
            tx_out.plutus_data() == Some(convert_plutus_data(datum.to_plutus_data()))
        })
        .expect("Utxo with inline datum not found");

    let collateral = utxos.keys().next().unwrap();

    let eq_datum_a_is_equal_tx = mk_claim_tx(
        &wallet.get_own_pkh(),
        &wallet.get_own_addr(),
        &utxos,
        eq_validator,
        &eq_validator_utxos,
        &tx_in,
        &collateral,
        &eq_redeemer,
    );

    let redeemer_data = convert_plutus_data(eq_redeemer.to_plutus_data());
    let datums = convert_plutus_data(datum.to_plutus_data());

    ogmios
        .balance_sign_and_submit_transacton(
            eq_datum_a_is_equal_tx,
            wallet,
            &wallet.get_own_addr(),
            &vec![eq_validator.clone()],
            &vec![redeemer_data],
            &vec![datums],
        )
        .await
}
