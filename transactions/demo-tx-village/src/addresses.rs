use lbf_demo_config_api::demo::config::{Config, Script};
use plutus_ledger_api::v3::address::{Address, Credential};
use tx_bakery::utils::script::ScriptOrRef;

pub fn addresses_eq_validator(config: Config, network: u8) {
    let Script(raw_script) = config.eq_validator;
    let (validator_hash, _) = ScriptOrRef::from_bytes_v3(raw_script)
        .expect(&format!("Couldn't deserialize Plutus Script"))
        .as_validator();

    let addr = Address {
        credential: Credential::Script(validator_hash.clone()),
        staking_credential: None,
    };

    let bech32_address: String = addr.with_extra_info(network).to_string();

    print!("{}", bech32_address);
}
