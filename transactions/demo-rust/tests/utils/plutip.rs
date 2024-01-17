use super::parse_pkh;
use cardano_serialization_lib::address::{Address, EnterpriseAddress, StakeCredential};
use cardano_serialization_lib::crypto::{Ed25519KeyHash, PrivateKey};
use data_encoding::HEXLOWER;
use derive_builder::Builder;
use std::fs;
use std::process::{Child, Command};
use std::{thread, time};

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

impl Plutip {
    const NETWORK_ID: u8 = 0b0001;

    pub fn start(config: &PlutipConfig) -> Self {
        let args = [
            "--dump-info-json",
            &config.dump_path,
            "--wallets-dir",
            &config.wallets_dir,
        ];

        let handler = Command::new("local-cluster")
            .args(args)
            .spawn()
            .expect("failed to execute plutip");

        thread::sleep(time::Duration::from_secs(5));
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

        PrivateKey::from_normal_bytes(
            &HEXLOWER
                .decode(&text_envelope.cbor_hex[4..].to_owned().into_bytes())
                .unwrap(),
        )
        .unwrap()
    }

    pub fn get_own_pkh(&self) -> Ed25519KeyHash {
        let pkh_str = &self.info.wallets[0].0;
        parse_pkh(pkh_str)
    }

    pub fn get_own_addr(&self) -> Address {
        EnterpriseAddress::new(
            Self::NETWORK_ID,
            &StakeCredential::from_keyhash(&self.get_own_pkh()),
        )
        .to_address()
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

    pub fn get_network_id(&self) -> u8 {
        Self::NETWORK_ID
    }

    pub fn kill(&mut self) -> Result<(), std::io::Error> {
        fs::remove_file(&self.config.dump_path)?;
        fs::remove_dir_all(&self.config.wallets_dir)?;
        self.handler.kill()
    }
}

impl Drop for Plutip {
    fn drop(&mut self) {
        let _ = self.kill();
    }
}
