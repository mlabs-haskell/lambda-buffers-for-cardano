use std::path::PathBuf;

use anyhow::anyhow;
use clap::Parser;
use demo_rust::{claim_eq_datum, lock_eq_datum};
use lbf_demo_config_api::demo::config::{Config, Script};
use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer, Product, Record, Sum};
use lbr_prelude::json::Json;
use plutus_ledger_api::v2::{
    address::{Address, Credential},
    crypto::LedgerBytes,
    script::{MintingPolicyHash, ScriptHash, ValidatorHash},
    value::{AssetClass, CurrencySymbol, TokenName},
};
use tokio::fs;
use tx_bakery::{
    chain_query::Network,
    utils::{
        key_wallet::KeyWallet,
        ogmios::client::{OgmiosClient, OgmiosClientConfig, OgmiosClientConfigBuilder},
        script::ScriptOrRef,
    },
};
use url::Url;

#[derive(Debug, Clone, Parser)]
struct OgmiosOpts {
    /// Address of the Ogmios service
    #[arg(long, value_name = "URL", default_value = "http://127.0.0.1:1337")]
    pub ogmios_url: Url,

    /// Cardano network type (mainnet | testnet|)
    #[arg(long, value_name = "NETWORK", default_value = "testnet")]
    pub network: Network,
}

impl TryFrom<OgmiosOpts> for OgmiosClientConfig {
    type Error = anyhow::Error;
    fn try_from(opts: OgmiosOpts) -> Result<OgmiosClientConfig, anyhow::Error> {
        OgmiosClientConfigBuilder::default()
            .url(opts.ogmios_url)
            .network(opts.network)
            .build()
            .map_err(|err| anyhow!("Couldn't build OgmiosClientConfig: {}", err))
    }
}

#[derive(Debug, Clone, Parser)]
struct KeyWalletOpts {
    /// Payment signing key
    #[arg(long, value_name = "FILE")]
    signing_key_file: PathBuf,

    /// Optional staking signing key
    #[arg(long, value_name = "FILE")]
    staking_signing_key_file: Option<PathBuf>,
}

#[derive(Debug, Clone, Parser)]
struct EnvOpts {
    #[command(flatten)]
    ogmios: OgmiosOpts,

    #[command(flatten)]
    key_wallet: KeyWalletOpts,
}

#[derive(Debug, Clone, Parser)]
enum Command {
    Lock(EnvOpts),
    Claim(EnvOpts),
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let command = Command::parse();
    let eq_validator = read_script("data/demo-plutarch-config.json")
        .await
        .as_validator();

    let eq_datum = setup_test_data(eq_validator.0.clone());

    match command {
        Command::Lock(env) => {
            let runtime = Runtime::init(env).await?;
            lock_eq_datum::build_and_submit(
                &runtime.wallet,
                &runtime.ogmios,
                &runtime.ogmios,
                eq_validator,
                &eq_datum,
            )
            .await
            .map_err(|err| anyhow!(err))?;
        }
        Command::Claim(env) => {
            let runtime = Runtime::init(env).await?;
            claim_eq_datum::build_and_submit(
                &runtime.wallet,
                &runtime.ogmios,
                &runtime.ogmios,
                eq_validator,
                &EqRedeemer::IsEqual(eq_datum.clone()),
                &eq_datum,
            )
            .await
            .map_err(|err| anyhow!(err))?;
        }
    };

    Ok(())
}

struct Runtime {
    ogmios: OgmiosClient,
    wallet: KeyWallet,
}

impl Runtime {
    async fn init(opts: EnvOpts) -> anyhow::Result<Runtime> {
        let ogmios_config = opts.ogmios.try_into()?;

        println!("Connecting to Ogmios");
        let ogmios = OgmiosClient::connect(ogmios_config)
            .await
            .map_err(|err| anyhow!(err))?;

        let wallet = KeyWallet::new(
            opts.key_wallet.signing_key_file,
            opts.key_wallet.staking_signing_key_file,
        )
        .await
        .map_err(|err| anyhow!(err))?;

        Ok(Runtime { ogmios, wallet })
    }
}

async fn read_script(path: &str) -> ScriptOrRef {
    let conf_str = fs::read_to_string(path).await.expect(&format!(
        "Couldn't read plutarch config JSON file at {}.",
        path
    ));

    let conf: Config = Json::from_json_string(&conf_str)
        .expect(&format!("Couldn't deserialize JSON data of file {}", path));

    let Script(raw_script) = conf.eq_validator;

    ScriptOrRef::from_bytes(raw_script).expect(&format!(
        "Couldn't deserialize PlutusScript of file {}.",
        path
    ))
}

fn setup_test_data(validator_hash: ValidatorHash) -> EqDatum {
    let example_token_name = TokenName::from_string("example token name");
    let example_currency_symbol =
        CurrencySymbol::NativeToken(MintingPolicyHash(ScriptHash(LedgerBytes([0].repeat(28)))));

    let example_asset_class = AssetClass {
        currency_symbol: example_currency_symbol,
        token_name: example_token_name,
    };
    let example_plutus_bytes = LedgerBytes(b"example bytes".to_vec());

    let example_address = Address {
        credential: Credential::Script(validator_hash),
        staking_credential: None,
    };

    EqDatum {
        rec: Record {
            bar: example_address.clone(),
            baz: example_plutus_bytes.clone(),
            foo: example_asset_class.clone(),
        },
        sum: Sum::Baz(example_plutus_bytes.clone()),
        prod: Product(
            example_asset_class.clone(),
            example_address.clone(),
            example_plutus_bytes.clone(),
        ),
    }
}
