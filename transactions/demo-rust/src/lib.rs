use crate::utils::{convert_plutus_data, create_tx_builder, to_redeemer};
use cardano_serialization_lib as csl;
use cardano_serialization_lib::output_builder::TransactionOutputBuilder;
use cardano_serialization_lib::tx_builder::tx_inputs_builder::{PlutusWitness, TxInputsBuilder};
use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer};
use plutus_ledger_api::plutus_data::IsPlutusData;
use std::collections::BTreeMap;

mod utils;

/// Transaction that stores a EqDatum value at the Eq Validator.
pub fn create_value_tx(
    validator_addr: &csl::address::Address,
    eq_datum: &EqDatum,
) -> Result<csl::TransactionBody, csl::error::JsError> {
    let mut tx_builder = create_tx_builder();
    let datum = convert_plutus_data(eq_datum.to_plutus_data());

    tx_builder
        .add_output(
            &TransactionOutputBuilder::new()
                .with_address(&validator_addr)
                .with_plutus_data(&datum)
                .next()
                .unwrap()
                .build()
                .unwrap(),
        )
        .unwrap();

    tx_builder.build()
}

// `inputIsEqualTx eqValidator eqValidatorUtxos txIn eqDatum` make a transaction that checks if the EqDatum stored at the EqValidator's `txIn` is equal to the provided one in `eqDatum`.
pub fn input_is_equal_tx(
    own_addr: &csl::address::Address,
    eq_validator: &csl::plutus::PlutusScript,
    eq_validator_utxos: BTreeMap<&csl::TransactionInput, &csl::TransactionOutput>,
    tx_input: &csl::TransactionInput,
    eq_datum: &EqDatum,
) -> Result<csl::TransactionBody, csl::error::JsError> {
    let mut tx_builder = create_tx_builder();
    let datum = convert_plutus_data(eq_datum.to_plutus_data());

    let redeemer = to_redeemer(convert_plutus_data(
        EqRedeemer::IsEqual(eq_datum.to_owned()).to_plutus_data(),
    ));
    let mut tx_inputs_builder = TxInputsBuilder::new();
    let value = eq_validator_utxos.get(tx_input).unwrap().amount();

    let tx_input_witness = PlutusWitness::new(eq_validator, &datum, &redeemer);
    tx_inputs_builder.add_plutus_script_input(&tx_input_witness, tx_input, &value);

    tx_builder
        .add_output(
            &TransactionOutputBuilder::new()
                .with_address(&own_addr)
                .next()
                .unwrap()
                .build()
                .unwrap(),
        )
        .unwrap();

    tx_builder.set_inputs(&tx_inputs_builder);

    tx_builder.build()
}

/// `inputIsNotEqualTx eqValidator eqValidatorUtxos txIn eqDatum` make a transaction that checks if the EqDatum stored at the EqValidator's `txIn` is NOT equal to the provided one in `eqDatum`.
pub fn input_is_not_equal_tx(
    own_addr: &csl::address::Address,
    eq_validator: &csl::plutus::PlutusScript,
    eq_validator_utxos: BTreeMap<&csl::TransactionInput, &csl::TransactionOutput>,
    tx_input: &csl::TransactionInput,
    eq_datum: &EqDatum,
) -> Result<csl::TransactionBody, csl::error::JsError> {
    let mut tx_builder = create_tx_builder();
    let datum = convert_plutus_data(eq_datum.to_plutus_data());

    let redeemer = to_redeemer(convert_plutus_data(
        EqRedeemer::IsNotEqual(eq_datum.to_owned()).to_plutus_data(),
    ));
    let mut tx_inputs_builder = TxInputsBuilder::new();
    let value = eq_validator_utxos.get(tx_input).unwrap().amount();

    let tx_input_witness = PlutusWitness::new(eq_validator, &datum, &redeemer);
    tx_inputs_builder.add_plutus_script_input(&tx_input_witness, tx_input, &value);

    tx_builder
        .add_output(
            &TransactionOutputBuilder::new()
                .with_address(&own_addr)
                .next()
                .unwrap()
                .build()
                .unwrap(),
        )
        .unwrap();

    tx_builder.set_inputs(&tx_inputs_builder);

    tx_builder.build()
}
