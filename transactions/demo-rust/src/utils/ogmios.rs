use super::wallet::Wallet;
use super::{to_int, to_redeemer};
use cardano_serialization_lib as csl;
use cardano_serialization_lib::address::Address;
use cardano_serialization_lib::output_builder::TransactionOutputBuilder;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};
use serde_json::value::Value as JsonValue;
use std::collections::BTreeMap;
use std::process::{Child, Command, Stdio};
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
            .stdout(Stdio::null())
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
                    csl::TransactionInput::try_from(utxo)
                        .expect("Couldn't convert transaction input"),
                    csl::TransactionOutput::try_from(utxo)
                        .expect("Couldn't convert transaction output"),
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

        resp.plutus_cost_models.iter().for_each(|(lang, costs)| {
            let mut cost_model = csl::plutus::CostModel::new();
            costs.iter().enumerate().for_each(|(index, model)| {
                let _ = cost_model.set(index, &to_int(*model));
            });
            let language = match &lang[..] {
                "plutus:v1" => csl::plutus::Language::new_plutus_v1(),
                "plutus:v2" => csl::plutus::Language::new_plutus_v2(),
                _ => panic!("Unknown Plutus language version"),
            };
            costmdls.insert(&language, &cost_model);
        });
        costmdls
    }

    pub async fn await_tx_confirm(&self, _tx_hash: &csl::crypto::TransactionHash) {
        // TODO: implement this
        tokio::time::sleep(std::time::Duration::from_secs(10)).await;
    }

    pub async fn evaluate_transaction(
        &self,
        tx_builder: &csl::tx_builder::TransactionBuilder,
        plutus_scripts: &Vec<csl::plutus::PlutusScript>,
        redeemers: &Vec<csl::plutus::PlutusData>,
    ) -> Vec<csl::plutus::ExUnits> {
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

        resp.into_iter()
            .map(|budgets| {
                csl::plutus::ExUnits::new(
                    &csl::utils::to_bignum(budgets.budget.memory),
                    &csl::utils::to_bignum(budgets.budget.cpu),
                )
            })
            .collect()
    }

    pub fn balance_transaction(
        &self,
        mut tx_builder: csl::tx_builder::TransactionBuilder,
        own_addr: &csl::address::Address,
        wit_redeemers: &Vec<csl::plutus::Redeemer>,
        wit_datums: &Vec<csl::plutus::PlutusData>,
        cost_models: &csl::plutus::Costmdls,
    ) -> csl::TransactionBody {
        let mut redeemers = csl::plutus::Redeemers::new();
        wit_redeemers.iter().for_each(|red| redeemers.add(&red));

        if !wit_datums.is_empty() || !wit_redeemers.is_empty() {
            let datums = if wit_datums.is_empty() {
                None
            } else {
                let mut ds = csl::plutus::PlutusList::new();
                wit_datums.iter().for_each(|dat| ds.add(&dat));
                Some(ds)
            };

            let mut used_langs = csl::plutus::Languages::new();
            used_langs.add(csl::plutus::Language::new_plutus_v2());

            let script_data_hash = csl::utils::hash_script_data(
                &redeemers,
                &cost_models.retain_language_versions(&used_langs),
                datums,
            );

            let _ = tx_builder.set_script_data_hash(&script_data_hash);
        }
        let _ = tx_builder.add_change_if_needed(own_addr);

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
        wallet: &impl Wallet,
        own_addr: &csl::address::Address,
        plutus_scripts: &Vec<csl::plutus::PlutusScript>,
        wit_redeemers: &Vec<csl::plutus::PlutusData>,
        wit_datums: &Vec<csl::plutus::PlutusData>,
    ) -> csl::crypto::TransactionHash {
        let ex_units = self
            .evaluate_transaction(&tx_builder, plutus_scripts, wit_redeemers)
            .await;

        let redeemers_w_ex_u = wit_redeemers
            .into_iter()
            .enumerate()
            .map(|(idx, redeemer_data)| {
                csl::plutus::Redeemer::new(
                    &csl::plutus::RedeemerTag::new_spend(),
                    &csl::utils::to_bignum(0),
                    &redeemer_data,
                    // &csl::plutus::ExUnits::new(
                    //     &csl::utils::to_bignum(1),
                    //     &csl::utils::to_bignum(2),
                    // ),
                    &ex_units[idx], // TODO: This is wrong, ex units should be matched by their validator pointers
                )
            })
            .collect::<Vec<csl::plutus::Redeemer>>();

        let cost_models = self.query_costmdls().await;

        let tx_body = self.balance_transaction(
            tx_builder,
            own_addr,
            &redeemers_w_ex_u,
            &wit_datums,
            &cost_models,
        );

        let tx = wallet.sign_transaction(&tx_body, plutus_scripts, &redeemers_w_ex_u);

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

#[derive(Debug, Builder, Clone)]
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

#[derive(Debug, Serialize, Deserialize)]
struct QueryLedgerStateUtxoParams {
    addresses: Vec<String>,
}

type QueryLedgerStateUtxoResponse = Vec<Utxo>;

#[derive(Debug, Serialize, Deserialize)]
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

#[derive(Debug, Serialize, Deserialize)]
struct TransactionId {
    id: String,
}

type Value = BTreeMap<String, BTreeMap<String, u64>>;

#[derive(Debug, Serialize, Deserialize)]
enum Script {
    Native(NativeScript),
    Plutus(PlutusScript),
}

#[derive(Debug, Serialize, Deserialize)]
struct NativeScript {
    language: String,
    json: JsonValue,
    cbor: String,
}

#[derive(Debug, Serialize, Deserialize)]
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

        let mut tx_builder = TransactionOutputBuilder::new();

        if let Some(datum_hash) = &resp.datum_hash {
            tx_builder = tx_builder.with_data_hash(
                &csl::crypto::DataHash::from_hex(&datum_hash).expect("Couldn't convert datum hash"),
            )
        }

        if let Some(datum) = &resp.datum {
            tx_builder = tx_builder.with_plutus_data(
                &csl::plutus::PlutusData::from_hex(&datum).expect("Couldn't convert datum"),
            )
        }

        if let Some(script) = &resp.script {
            let script_ref = match script {
                Script::Native(native_script) => csl::ScriptRef::new_native_script(
                    &csl::NativeScript::from_hex(&native_script.cbor)
                        .expect("Couldn't convert native script"),
                ),
                Script::Plutus(plutus_script) => csl::ScriptRef::new_plutus_script(
                    &csl::plutus::PlutusScript::from_hex(&plutus_script.cbor)
                        .expect("Couldn't convert Plutus script"),
                ),
            };

            tx_builder = tx_builder.with_script_ref(&script_ref)
        }

        tx_builder
            .with_address(
                &csl::address::Address::from_bech32(&resp.address)
                    .expect("Couldn't convert address"),
            )
            .next()
            .unwrap()
            .with_value(&csl::utils::Value::new_with_assets(
                &csl::utils::to_bignum(coins),
                &assets,
            ))
            .build()
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct EvaluateTransactionParams {
    transaction: TransactionCbor,
    #[serde(rename(deserialize = "additionalUtxo"))]
    additional_utxo: Vec<Utxo>,
}

#[derive(Debug, Serialize, Deserialize)]
struct TransactionCbor {
    cbor: String,
}

type EvaluateTransactionResponse = Vec<Budgets>;

#[derive(Debug, Serialize, Deserialize)]
pub struct Budgets {
    pub validator: String,
    pub budget: Budget,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Budget {
    pub memory: u64,
    pub cpu: u64,
}

#[derive(Debug, Serialize, Deserialize)]
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

#[derive(Debug, Serialize, Deserialize)]
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

#[derive(Debug, Serialize, Deserialize)]
struct JsonRPCError {
    code: i16,
    message: String,
    data: Option<JsonValue>, // TODO
}

type SubmitTransactionParams = EvaluateTransactionParams;

#[derive(Debug, Serialize, Deserialize)]
pub struct SubmitTransactionResponse {
    transaction: TransactionId,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct QueryLedgerStateProtocolParametersResponse {
    #[serde(rename(deserialize = "plutusCostModels"))]
    plutus_cost_models: BTreeMap<String, Vec<i64>>,
    // TODO: Rest of the params was not needed yet
}
