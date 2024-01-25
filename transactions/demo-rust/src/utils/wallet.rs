use cardano_serialization_lib::address::Address;
use cardano_serialization_lib::crypto::Ed25519KeyHash;
use cardano_serialization_lib::plutus::{PlutusScript, Redeemer};
use cardano_serialization_lib::{Transaction, TransactionBody};

pub trait Wallet {
    /// Signs a fully built transaction and appends this together with redeemers and scripts to the
    /// witness set
    fn sign_transaction(
        &self,
        tx_body: &TransactionBody,
        plutus_scripts: &Vec<PlutusScript>,
        redeemers: &Vec<Redeemer>,
    ) -> Transaction;

    /// Query the public key hash used by this wallet
    fn get_own_pkh(&self) -> Ed25519KeyHash;

    /// Query the wallet address
    fn get_own_addr(&self) -> Address;

    /// Query the network id (not identical to network magic)
    fn get_network_id(&self) -> u8;
}
