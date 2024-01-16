use super::parse_pkh;
use cardano_serialization_lib::crypto::Ed25519KeyHash;
use derive_builder::Builder;
use std::fs;
use std::process::{Child, Command};
use std::{thread, time};

pub struct Plutip {
    handler: Child,
    config: PlutipConfig,
    info: PlutipInfo,
}

impl Plutip {
    pub fn start(config: &PlutipConfig) -> Self {
        let args = ["--dump-info-json", &config.dump_path];

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

    pub fn get_pkh(&self) -> Ed25519KeyHash {
        let pkh_str = &self.info.wallets[0].0;
        parse_pkh(pkh_str)
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

    pub fn kill(&mut self) -> Result<(), std::io::Error> {
        fs::remove_file(&self.config.dump_path)?;
        self.handler.kill()
    }
}

impl Drop for Plutip {
    fn drop(&mut self) {
        let _ = self.kill();
    }
}

#[derive(serde::Deserialize)]
struct PlutipInfo {
    #[serde(rename(deserialize = "ciNodeSocket"))]
    node_socket: String,
    #[serde(rename(deserialize = "ciWallets"))]
    wallets: Vec<(String, String)>,
}

#[derive(Builder, Clone)]
pub struct PlutipConfig {
    #[builder(default = r#""plutip-info.json".to_string()"#, setter(skip))]
    dump_path: String,
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
