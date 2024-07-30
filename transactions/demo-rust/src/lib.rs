use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer};
use num_bigint::BigInt;
use plutus_ledger_api::{
    plutus_data::IsPlutusData,
    v2::{
        address::{Address, Credential},
        datum::Datum,
        datum::OutputDatum,
        redeemer::Redeemer,
        script::ValidatorHash,
        transaction::TxInInfo,
        transaction::{TransactionHash, TransactionInfo, TransactionInput, TransactionOutput},
        value::Value,
    },
};
use std::collections::BTreeMap;
use tx_bakery::{
    chain_query::{ChainQuery, FullTransactionOutput},
    error::Result,
    submitter::Submitter,
    tx_info_builder::TxScaffold,
    utils::script::ScriptOrRef,
    wallet::Wallet,
};
use tx_bakery::{ChangeStrategy, CollateralStrategy, TxBakery, TxWithCtx};

/// Transaction that stores a EqDatum value at the Eq Validator.
pub mod lock_eq_datum {

    use super::*;

    pub fn mk_tx_info(
        validator_addr: &Address,
        eq_datum: &EqDatum,
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
    ) -> TransactionInfo {
        let datum = Datum(eq_datum.to_plutus_data());
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        TxScaffold::new()
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            .add_output(TransactionOutput {
                address: validator_addr.clone(),
                value: Value::new(),
                datum: OutputDatum::InlineDatum(datum),
                reference_script: None,
            })
            .build()
    }

    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        eq_validator: (ValidatorHash, ScriptOrRef),
        example_eq_datum: &EqDatum,
    ) -> Result<TransactionHash> {
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        let eq_validator_addr = Address {
            credential: Credential::Script(eq_validator.0.clone()),
            staking_credential: None,
        };

        let tx_info = mk_tx_info(&eq_validator_addr, example_eq_datum, &utxos);

        let scripts = BTreeMap::new();

        let tx_bakery = TxBakery::init(chain_query).await?;
        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());

        let tx = TxWithCtx::new(
            &tx_info,
            &scripts,
            &CollateralStrategy::None,
            &change_strategy,
        );

        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}

/// Make a transaction that releases UTxO stored at the `EqValidator` in one of the cases below:
/// - redeemer is `IsEqual` and the supplied plutus data is equal to the one locked as datum
/// - redeemer is `IsNotEqual` and the supplied plutus data is not equal to the one locked as datum
pub mod claim_eq_datum {

    use super::*;

    pub fn mk_tx_info(
        own_addr: &Address,
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
        tx_input: (&TransactionInput, &FullTransactionOutput),
        eq_redeemer: &EqRedeemer,
    ) -> TransactionInfo {
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");
        let redeemer = Redeemer(eq_redeemer.to_plutus_data());

        TxScaffold::new()
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            .add_script_input(tx_input.0.clone(), tx_input.1.into(), None, redeemer)
            .add_output(TransactionOutput {
                address: own_addr.clone(),
                value: Value::new(),
                datum: OutputDatum::None,
                reference_script: None,
            })
            .build()
    }

    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        eq_validator: (ValidatorHash, ScriptOrRef),
        eq_redeemer: &EqRedeemer,
        datum: &EqDatum,
    ) -> Result<TransactionHash> {
        let eq_validator_addr = Address {
            credential: Credential::Script(eq_validator.0.clone()),
            staking_credential: None,
        };

        let eq_validator_utxos = chain_query.query_utxos_by_addr(&eq_validator_addr).await?;
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        let tx_in = eq_validator_utxos
            .iter()
            .find(|(_, tx_out)| {
                if let OutputDatum::InlineDatum(Datum(inline_datum)) = &tx_out.datum {
                    EqDatum::from_plutus_data(&inline_datum).unwrap() == *datum
                } else {
                    false
                }
            })
            .expect("Utxo with inline datum not found");

        let tx_info = mk_tx_info(&wallet.get_change_addr(), &utxos, tx_in, &eq_redeemer);

        let scripts = BTreeMap::from([eq_validator.1.with_script_hash()]);

        let collateral_utxo = utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let collateral = CollateralStrategy::Explicit(TxInInfo {
            reference: collateral_utxo.0.clone(),
            output: collateral_utxo.1.into(),
        });

        let tx_bakery = TxBakery::init(chain_query).await?;

        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());
        let tx = TxWithCtx::new(&tx_info, &scripts, &collateral, &change_strategy);

        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}
