use cardano_serialization_lib as csl;
use cardano_serialization_lib::address::Address;
use cardano_serialization_lib::output_builder::TransactionOutputBuilder;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};
use serde_json::value::RawValue;
use std::collections::BTreeMap;
use std::process::{Child, Command};
use std::{thread, time};
use uuid::Uuid;

pub struct Ogmios {
    handler: Child,
    config: OgmiosConfig,
    client: reqwest::Client,
}

impl Ogmios {
    pub fn start(config: &OgmiosConfig) -> Self {
        let args = [
            "--node-socket",
            &config.node_socket,
            "--node-config",
            &config.node_config,
        ];

        let handler = Command::new("ogmios")
            .args(args)
            .spawn()
            .expect("failed to execute ogmios");

        let client = reqwest::Client::new();

        thread::sleep(time::Duration::from_secs(5));
        Self {
            handler,
            config: config.clone(),
            client,
        }
    }

    pub fn kill(&mut self) -> Result<(), std::io::Error> {
        self.handler.kill()
    }

    pub async fn query_utxos(
        &self,
        address: &Address,
    ) -> BTreeMap<csl::TransactionInput, csl::TransactionOutput> {
        let addr = &address.to_bech32(Some("addr".to_owned())).unwrap();
        let params = QueryLedgerStateUtxoParams {
            addresses: vec![addr.to_string()],
        };

        let resp: QueryLedgerStateUtxoResponse =
            self.request("queryLedgerState/utxo", params).await;

        resp.iter()
            .map(|utxo| {
                (
                    csl::TransactionInput::try_from(utxo).unwrap(),
                    csl::TransactionOutput::try_from(utxo).unwrap(),
                )
            })
            .collect()
    }

    pub async fn evaluate_transaction(&self, tx_body: &csl::TransactionBody) {
        let tx = csl::Transaction::new(tx_body, &csl::TransactionWitnessSet::new(), None);
        let params = EvaluateTransactionParams {
            transaction: TransactionCbor { cbor: tx.to_hex() },
            additional_utxo: Vec::new(),
        };

        let resp: EvaluateTransactionResponse = self.request("evaluateTransaction", params).await;

        ()
    }

    async fn request<T, U>(&self, method: &str, params: T) -> U
    where
        T: Serialize,
        U: serde::de::DeserializeOwned + Serialize,
    {
        let url = format!(
            "http://{}:{}",
            self.config.host,
            self.config.port.to_string()
        );
        let resp: JsonRPCResponse<U> = self
            .client
            .post(url)
            .json(&JsonRPCRequest::new(method, params))
            .send()
            .await
            .unwrap()
            .json()
            .await
            .unwrap();

        resp.result
    }
}

impl Drop for Ogmios {
    fn drop(&mut self) {
        let _ = self.kill();
    }
}

#[derive(Builder, Clone)]
pub struct OgmiosConfig {
    #[builder(default = r#""".to_string()"#)]
    node_socket: String,
    #[builder(default = r#"".node.config".to_string()"#)]
    node_config: String,
    #[builder(default = r#""127.0.0.1".to_string()"#)]
    host: String,
    #[builder(default = "1337")]
    port: u32,
    #[builder(default = "90")]
    _timeout: u32,
    #[builder(default = "1000")]
    _max_in_flight: u32,
}

#[derive(Serialize)]
struct QueryLedgerStateUtxoParams {
    addresses: Vec<String>,
}

type QueryLedgerStateUtxoResponse = Vec<Utxo>;

#[derive(Serialize, Deserialize)]
struct Utxo {
    transaction: TransactionId,
    index: u32,
    address: String,
    value: Value,
    #[serde(rename(deserialize = "datumHash"))]
    datum_hash: Option<String>,
    datum: Option<String>,
    script: Option<Script>,
}

#[derive(Serialize, Deserialize)]
struct TransactionId {
    id: String,
}

type Value = BTreeMap<String, BTreeMap<String, u64>>;

#[derive(Serialize, Deserialize)]
enum Script {
    Native(NativeScript),
    Plutus(PlutusScript),
}

#[derive(Serialize, Deserialize)]
struct NativeScript {
    language: String,
    json: Box<RawValue>,
    cbor: String,
}

#[derive(Serialize, Deserialize)]
struct PlutusScript {
    language: String,
    cbor: String,
}

impl TryFrom<&Utxo> for csl::TransactionInput {
    type Error = csl::error::JsError;

    fn try_from(resp: &Utxo) -> Result<csl::TransactionInput, Self::Error> {
        Ok(csl::TransactionInput::new(
            &csl::crypto::TransactionHash::from_hex(&resp.transaction.id)?,
            resp.index,
        ))
    }
}

impl TryFrom<&Utxo> for csl::TransactionOutput {
    type Error = csl::error::JsError;

    fn try_from(resp: &Utxo) -> Result<csl::TransactionOutput, Self::Error> {
        let coins = *resp
            .value
            .get("ada")
            .and_then(|x| x.get("lovelace"))
            .unwrap_or(&0);

        // TODO(szg251): This whole thing
        let assets = csl::MultiAsset::new();

        TransactionOutputBuilder::new()
            .with_address(&csl::address::Address::from_bech32(&resp.address).unwrap())
            .next()
            .unwrap()
            .with_value(&csl::utils::Value::new_with_assets(
                &csl::utils::to_bignum(coins),
                &assets,
            ))
            .build()
    }
}

#[derive(Deserialize, Serialize)]
struct EvaluateTransactionParams {
    transaction: TransactionCbor,
    #[serde(rename(deserialize = "additionalUtxo"))]
    additional_utxo: Vec<Utxo>,
}

#[derive(Deserialize, Serialize)]
struct TransactionCbor {
    cbor: String,
}

type EvaluateTransactionResponse = Vec<Budgets>;

#[derive(Deserialize, Serialize)]
struct Budgets {
    validator: String,
    budget: Budget,
}

#[derive(Deserialize, Serialize)]
struct Budget {
    memory: u64,
    cpu: u64,
}

#[derive(Serialize)]
struct JsonRPCRequest<T: Serialize> {
    jsonrpc: String,
    method: String,
    params: T,
    id: String,
}

impl<T: Serialize> JsonRPCRequest<T> {
    pub fn new(method: &str, params: T) -> Self {
        Self {
            jsonrpc: "2.0".to_owned(),
            method: method.to_owned(),
            params,
            id: Uuid::new_v4().to_string(),
        }
    }
}

#[derive(Deserialize, Serialize)]
struct JsonRPCResponse<T: Serialize> {
    jsonrpc: String,
    method: String,
    result: T,
    id: String,
}
