use cardano_serialization_lib as csl;
use cardano_serialization_lib::address::Address;
use cardano_serialization_lib::output_builder::TransactionOutputBuilder;
use demo_rust::utils::{sign_transaction, to_int, to_redeemer};
use derive_builder::Builder;
use serde::{Deserialize, Serialize};
use serde_json::value::Value as JsonValue;
use std::collections::BTreeMap;
use std::process::{Child, Command};
use std::time;
use uuid::Uuid;

pub struct Ogmios {
    handler: Child,
    config: OgmiosConfig,
    client: reqwest::Client,
}

impl Ogmios {
    pub async fn start(config: &OgmiosConfig) -> Self {
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

        // TODO This is less than ideal, we should do a proper healthcheck
        tokio::time::sleep(time::Duration::from_secs(5)).await;
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

        let resp: QueryLedgerStateUtxoResponse = self
            .request("queryLedgerState/utxo", Some(params))
            .await
            .unwrap();

        resp.iter()
            .map(|utxo| {
                (
                    csl::TransactionInput::try_from(utxo).unwrap(),
                    csl::TransactionOutput::try_from(utxo).unwrap(),
                )
            })
            .collect()
    }

    pub async fn query_costmdls(&self) -> csl::plutus::Costmdls {
        let resp = self
            .request::<(), QueryLedgerStateProtocolParametersResponse>(
                "queryLedgerState/protocolParameters",
                None,
            )
            .await
            .unwrap();

        let mut costmdls = csl::plutus::Costmdls::new();

        let mut plutus_v1 = csl::plutus::CostModel::new();
        let mut plutus_v2 = csl::plutus::CostModel::new();

        resp.plutus_cost_models
            .get("plutus:v1")
            .unwrap()
            .iter()
            .enumerate()
            .for_each(|(index, model)| {
                let _ = plutus_v1.set(index, &to_int(*model));
            });
        resp.plutus_cost_models
            .get("plutus:v1")
            .unwrap()
            .iter()
            .enumerate()
            .for_each(|(index, model)| {
                let _ = plutus_v2.set(index, &to_int(*model));
            });
        costmdls.insert(&csl::plutus::Language::new_plutus_v1(), &plutus_v1);
        costmdls.insert(&csl::plutus::Language::new_plutus_v2(), &plutus_v2);
        costmdls
    }

    pub async fn evaluate_transaction(
        &self,
        tx_builder: &csl::tx_builder::TransactionBuilder,
        plutus_scripts: Vec<&csl::plutus::PlutusScript>,
        redeemers: Vec<&csl::plutus::PlutusData>,
    ) -> csl::plutus::ExUnits {
        let mut tx_builder = tx_builder.clone();

        tx_builder.set_fee(&csl::utils::to_bignum(0));

        let mut witness_set = csl::TransactionWitnessSet::new();

        let mut script_witnesses = csl::plutus::PlutusScripts::new();

        plutus_scripts
            .iter()
            .for_each(|script| script_witnesses.add(script));

        let mut redeemer_witnesses = csl::plutus::Redeemers::new();

        redeemers
            .iter()
            .for_each(|redeemer| redeemer_witnesses.add(&to_redeemer(redeemer)));

        witness_set.set_plutus_scripts(&script_witnesses);
        witness_set.set_redeemers(&redeemer_witnesses);

        let tx_body = tx_builder.build().unwrap();
        let tx = csl::Transaction::new(&tx_body, &witness_set, None);

        let params = EvaluateTransactionParams {
            transaction: TransactionCbor { cbor: tx.to_hex() },
            additional_utxo: Vec::new(),
        };

        let resp: EvaluateTransactionResponse = self
            .request("evaluateTransaction", Some(params))
            .await
            .unwrap();

        let (mem, cpu) = resp.into_iter().fold((0, 0), |(mem, cpu), budgets| {
            (mem + budgets.budget.memory, cpu + budgets.budget.cpu)
        });
        csl::plutus::ExUnits::new(&csl::utils::to_bignum(mem), &csl::utils::to_bignum(cpu))
    }

    pub async fn balance_transaction(
        &self,
        mut tx_builder: csl::tx_builder::TransactionBuilder,
        own_addr: &csl::address::Address,
        cost_models: &csl::plutus::Costmdls,
    ) -> csl::TransactionBody {
        let _ = tx_builder.add_change_if_needed(own_addr);
        tx_builder.calc_script_data_hash(&cost_models);
        tx_builder.build().unwrap()
    }

    pub async fn submit_transaction(&self, tx: &csl::Transaction) -> csl::crypto::TransactionHash {
        let params = SubmitTransactionParams {
            transaction: TransactionCbor { cbor: tx.to_hex() },
            additional_utxo: Vec::new(),
        };

        let resp: SubmitTransactionResponse = self
            .request("submitTransaction", Some(params))
            .await
            .unwrap();

        csl::crypto::TransactionHash::from_hex(&resp.transaction.id).unwrap()
    }

    pub async fn balance_sign_and_submit_transacton(
        &self,
        tx_builder: csl::tx_builder::TransactionBuilder,
        priv_key: &csl::crypto::PrivateKey,
        own_addr: &csl::address::Address,
        plutus_scripts: Vec<&csl::plutus::PlutusScript>,
        redeemers: Vec<&csl::plutus::Redeemer>,
        cost_models: &csl::plutus::Costmdls,
    ) -> csl::crypto::TransactionHash {
        let tx_body = self
            .balance_transaction(tx_builder, own_addr, cost_models)
            .await;

        let tx = sign_transaction(&tx_body, priv_key, plutus_scripts, redeemers);

        self.submit_transaction(&tx).await
    }

    async fn request<T, U>(&self, method: &str, params: Option<T>) -> Result<U, JsonRPCError>
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

        match resp {
            JsonRPCResponse::Success { result, .. } => Ok(result),
            JsonRPCResponse::Error { error, .. } => Err(error),
        }
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

#[derive(Serialize, Deserialize)]
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
    json: JsonValue,
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

#[derive(Serialize, Deserialize)]
struct EvaluateTransactionParams {
    transaction: TransactionCbor,
    #[serde(rename(deserialize = "additionalUtxo"))]
    additional_utxo: Vec<Utxo>,
}

#[derive(Serialize, Deserialize)]
struct TransactionCbor {
    cbor: String,
}

type EvaluateTransactionResponse = Vec<Budgets>;

#[derive(Serialize, Deserialize)]
pub struct Budgets {
    pub validator: String,
    pub budget: Budget,
}

#[derive(Serialize, Deserialize)]
pub struct Budget {
    pub memory: u64,
    pub cpu: u64,
}

#[derive(Serialize, Deserialize)]
struct JsonRPCRequest<T: Serialize> {
    jsonrpc: String,
    method: String,
    params: Option<T>,
    id: String,
}

impl<T: Serialize> JsonRPCRequest<T> {
    pub fn new(method: &str, params: Option<T>) -> Self {
        Self {
            jsonrpc: "2.0".to_owned(),
            method: method.to_owned(),
            params,
            id: Uuid::new_v4().to_string(),
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum JsonRPCResponse<T: Serialize> {
    Success {
        jsonrpc: String,
        method: String,
        result: T,
        id: String,
    },
    Error {
        jsonrpc: String,
        method: String,
        error: JsonRPCError,
        id: String,
    },
}

#[derive(Serialize, Deserialize, Debug)]
struct JsonRPCError {
    code: i16,
    message: String,
    data: Option<JsonValue>, // TODO
}

type SubmitTransactionParams = EvaluateTransactionParams;

#[derive(Serialize, Deserialize)]
pub struct SubmitTransactionResponse {
    transaction: TransactionId,
}

#[derive(Serialize, Deserialize)]
pub struct QueryLedgerStateProtocolParametersResponse {
    #[serde(rename(deserialize = "plutusCostModels"))]
    plutus_cost_models: BTreeMap<String, Vec<i64>>,
    // TODO: Rest of the params was not needed yet
}
