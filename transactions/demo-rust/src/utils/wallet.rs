use cardano_serialization_lib::address::Address;
use cardano_serialization_lib::crypto::Ed25519KeyHash;
use cardano_serialization_lib::plutus::{PlutusScript, Redeemer};
use cardano_serialization_lib::{Transaction, TransactionBody};

pub trait Wallet {
    fn sign_transaction(
        &self,
        tx_body: &TransactionBody,
        plutus_scripts: Vec<&PlutusScript>,
        redeemers: Vec<&Redeemer>,
    ) -> Transaction;

    fn get_own_pkh(&self) -> Ed25519KeyHash;

    fn get_own_addr(&self) -> Address;

    fn get_network_id(&self) -> u8;
}
