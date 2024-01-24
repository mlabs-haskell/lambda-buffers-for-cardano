use cardano_serialization_lib::address::{Address, EnterpriseAddress, StakeCredential};
use cardano_serialization_lib::crypto::Vkeywitnesses;
use cardano_serialization_lib::crypto::{Ed25519KeyHash, PrivateKey};
use cardano_serialization_lib::plutus::{PlutusScript, PlutusScripts, Redeemer, Redeemers};
use cardano_serialization_lib::utils::{hash_transaction, make_vkey_witness};
use cardano_serialization_lib::{Transaction, TransactionBody, TransactionWitnessSet};
use data_encoding::HEXLOWER;
use demo_rust::utils::wallet::Wallet;
use derive_builder::Builder;
use std::fs;
use std::io::Cursor;
use std::process::Stdio;
use std::time;
use tokio;
use tokio::process::{Child, Command};

#[derive(Builder, Clone)]
pub struct PlutipConfig {
    #[builder(default = r#""plutip-info.json".to_string()"#, setter(skip))]
    dump_path: String,
    #[builder(default = r#"".wallets".to_string()"#, setter(skip))]
    wallets_dir: String,
    #[builder(default = "1")]
    _wallets: u32,
    #[builder(default = "1")]
    _ada: u32,
    #[builder(default = "1")]
    _lovelace: u32,
    #[builder(default = "1")]
    _utxos: u32,
    #[builder(default = "1")]
    _slot_length: u32,
    #[builder(default = "1")]
    _epoch_size: u32,
}

#[derive(serde::Deserialize)]
struct PlutipInfo {
    #[serde(rename(deserialize = "ciNodeSocket"))]
    node_socket: String,
    #[serde(rename(deserialize = "ciWallets"))]
    wallets: Vec<(String, String)>,
}

#[derive(serde::Deserialize)]
struct TextEnvelope {
    #[serde(rename(deserialize = "type"))]
    data_type: String,
    description: String,
    #[serde(rename(deserialize = "cborHex"))]
    cbor_hex: String,
}

pub struct Plutip {
    handler: Child,
    config: PlutipConfig,
    info: PlutipInfo,
}

impl Wallet for Plutip {
    fn sign_transaction(
        &self,
        tx_body: &TransactionBody,
        plutus_scripts: &Vec<PlutusScript>,
        redeemers: &Vec<Redeemer>,
    ) -> Transaction {
        let mut witness_set = TransactionWitnessSet::new();
        let mut vkey_witnesses = Vkeywitnesses::new();
        vkey_witnesses.add(&make_vkey_witness(
            &hash_transaction(tx_body),
            &self.get_priv_key(),
        ));

        let mut script_witnesses = PlutusScripts::new();

        plutus_scripts
            .iter()
            .for_each(|script| script_witnesses.add(script));

        let mut redeemer_witnesses = Redeemers::new();

        redeemers
            .iter()
            .for_each(|redeemer| redeemer_witnesses.add(redeemer));

        witness_set.set_vkeys(&vkey_witnesses);
        witness_set.set_plutus_scripts(&script_witnesses);
        witness_set.set_redeemers(&redeemer_witnesses);

        Transaction::new(&tx_body, &witness_set, None)
    }
    fn get_own_pkh(&self) -> Ed25519KeyHash {
        let pkh_str = &self.info.wallets[0].0;

        Ed25519KeyHash::from_bytes(HEXLOWER.decode(&pkh_str.to_owned().into_bytes()).unwrap())
            .unwrap()
    }

    fn get_own_addr(&self) -> Address {
        EnterpriseAddress::new(
            Self::NETWORK_ID,
            &StakeCredential::from_keyhash(&self.get_own_pkh()),
        )
        .to_address()
    }

    fn get_network_id(&self) -> u8 {
        Self::NETWORK_ID
    }
}

impl Plutip {
    const NETWORK_ID: u8 = 0b0001;

    pub async fn start(config: &PlutipConfig) -> Self {
        let args = [
            "--dump-info-json",
            &config.dump_path,
            "--wallets-dir",
            &config.wallets_dir,
        ];

        let handler = Command::new("local-cluster")
            .args(args)
            .stdout(Stdio::null())
            .kill_on_drop(true)
            .spawn()
            .expect("failed to execute plutip");

        // TODO This is less than ideal, we should do a proper healthcheck
        tokio::time::sleep(time::Duration::from_secs(10)).await;
        let info = Self::fetch_info(&config.dump_path);
        Self {
            handler,
            config: config.clone(),
            info,
        }
    }

    fn fetch_info(path: &str) -> PlutipInfo {
        let info_str = fs::read_to_string(path)
            .expect(&format!("Couldn't read Plutip info JSON file at {}.", path));

        serde_json::from_str(&info_str)
            .expect(&format!("Couldn't deserialize JSON data of file {}", path))
    }

    pub fn get_priv_key(&self) -> PrivateKey {
        let path = format!(
            "{}/signing-key-{}.skey",
            self.config.wallets_dir,
            self.get_own_pkh().to_string()
        );
        let skey_str = fs::read_to_string(&path)
            .expect(&format!("Couldn't read wallet signing key at {}.", &path));

        let text_envelope: TextEnvelope = serde_json::from_str(&skey_str)
            .expect(&format!("Couldn't deserialize JSON data of file {}", &path));

        let mut raw = cbor_event::de::Deserializer::from(Cursor::new(
            HEXLOWER
                .decode(&text_envelope.cbor_hex.clone().into_bytes())
                .unwrap(),
        ));
        let bytes: Vec<u8> = raw.bytes().unwrap();

        PrivateKey::from_normal_bytes(&bytes).unwrap()
    }

    pub fn get_node_socket(&self) -> String {
        self.info.node_socket.clone()
    }

    pub fn get_node_config_path(&self) -> String {
        let mut path = fs::canonicalize(&self.info.node_socket).unwrap();
        path.pop();
        path.pop();
        path.push("pool-1");
        path.push("node.config");

        path.to_str().unwrap().to_string()
    }

    pub async fn kill(&mut self) -> Result<(), std::io::Error> {
        self.cleanup()?;
        self.handler.kill().await
    }

    pub fn cleanup(&mut self) -> Result<(), std::io::Error> {
        fs::remove_file(&self.config.dump_path)?;
        fs::remove_dir_all(&self.config.wallets_dir)
    }
}

impl Drop for Plutip {
    fn drop(&mut self) {
        self.cleanup().expect("Failed to clean up after Plutip.")
    }
}
