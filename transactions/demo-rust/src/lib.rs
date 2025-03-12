use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer};
use num_bigint::BigInt;
use plutus_ledger_api::{
    plutus_data::IsPlutusData,
    v3::{
        address::{Address, Credential},
        datum::{Datum, OutputDatum},
        redeemer::Redeemer,
        script::ValidatorHash,
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
    ChangeStrategy, CollateralStrategy, TxBakery, TxWithCtx,
};

/// Transaction that stores a EqDatum value at the Eq Validator.
pub mod lock_eq_datum {

    use super::*;

    pub fn mk_tx_info(
        validator_addr: &Address,
        eq_datum: &EqDatum,
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
    ) -> TransactionInfo {
        // Converting datum to PlutusData
        let datum = Datum(eq_datum.to_plutus_data());

        // Find fee input UTxO: in this case, pick a random UTxO with more than 5 Ada
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        TxScaffold::new()
            // Adding fee input from our pub key address
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            // Creating an output at the validator address with an inline datum
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
        println!("Build lock transaction");

        // Fetching the UTxOs from the chain query client at our address
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        // Converting the validator hash into an address
        let eq_validator_addr = Address {
            credential: Credential::Script(eq_validator.0.clone()),
            staking_credential: None,
        };

        // Calling our previously implemented TxInfo builder
        // Alternatively we could call out to an external service to get the TxInfo
        let tx_info = mk_tx_info(&eq_validator_addr, example_eq_datum, &utxos);

        // Creating a map of all the scripts used in the transaction (unused scripts won't be
        // attached)
        let scripts = BTreeMap::new();

        // Initialise TxBakery by fetching protocol parameters from the ChainQuery
        let tx_bakery = TxBakery::init(chain_query).await?;

        // Define the strategy to handle change outpu
        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());

        // Transaction with context will attach required scripts, collateral, etc.
        let tx = TxWithCtx::new(
            &tx_info,
            &scripts,
            // Define the strategy to find a suitable collateral (no script is executed, so no need
            // to add a collateral input)
            &CollateralStrategy::None,
            &change_strategy,
        );

        println!("Bake and deliver lock transaction");
        // Bake, sign and submit the transaction
        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}

/// Make a transaction that releases UTxO stored at the `EqValidator` in one of the cases below:
/// - redeemer is `IsEqual` and the supplied plutus data is equal to the one locked as datum
/// - redeemer is `IsNotEqual` and the supplied plutus data is not equal to the one locked as datum
pub mod claim_eq_datum {

    use super::*;

    pub fn mk_tx_info(
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
        eq_validator_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
        eq_redeemer: &EqRedeemer,
        eq_datum: &EqDatum,
    ) -> TransactionInfo {
        // Find fee input UTxO: in this case, pick a random UTxO with more than 5 Ada
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        // Finding the locked UTxO with the correct inline datum
        let tx_input = eq_validator_utxos
            .iter()
            .find(|(_, tx_out)| {
                if let OutputDatum::InlineDatum(Datum(inline_datum)) = &tx_out.datum {
                    EqDatum::from_plutus_data(inline_datum).unwrap() == *eq_datum
                } else {
                    false
                }
            })
            .expect("UTxO with inline datum not found");

        // Converting redeemer to PlutusData
        let redeemer = Redeemer(eq_redeemer.to_plutus_data());

        TxScaffold::new()
            // Adding fee input from our pub key address
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            // Input from the validator
            .add_script_input(tx_input.0.clone(), tx_input.1.into(), None, redeemer)
            // Build a TransactionInfo
            .build()
    }

    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        eq_validator: (ValidatorHash, ScriptOrRef),
        eq_redeemer: &EqRedeemer,
        eq_datum: &EqDatum,
    ) -> Result<TransactionHash> {
        println!("Build claim transaction");

        // Converting the validator hash into an address
        let eq_validator_addr = Address {
            credential: Credential::Script(eq_validator.0.clone()),
            staking_credential: None,
        };

        // Fetching the UTxOs from the chain query client at our address
        let own_utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        // Fetching the UTxOs from the chain query client at the validator address
        let eq_validator_utxos = chain_query.query_utxos_by_addr(&eq_validator_addr).await?;

        // Calling our previously implemented TxInfo builder
        // Alternatively we could call out to an external service to get the TxInfo
        let tx_info = mk_tx_info(&own_utxos, &eq_validator_utxos, eq_redeemer, eq_datum);

        // Creating a map of all the scripts used in the transaction (unused scripts won't be
        // attached)
        let scripts = BTreeMap::from([eq_validator.1.with_script_hash()]);

        // Define the strategy to find a suitable collateral
        let collateral = CollateralStrategy::Automatic {
            min_amount: 5_000_000,
            max_utxo_count: 1,
        };

        // Initialise TxBakery by fetching protocol parameters from the ChainQuery
        let tx_bakery = TxBakery::init(chain_query).await?;

        // Define the strategy to handle change outpu
        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());

        // Transaction with context will attach required scripts, collateral, etc.
        let tx = TxWithCtx::new(&tx_info, &scripts, &collateral, &change_strategy);

        println!("Bake and deliver claim transaction");
        // Bake, sign and submit the transaction
        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}
