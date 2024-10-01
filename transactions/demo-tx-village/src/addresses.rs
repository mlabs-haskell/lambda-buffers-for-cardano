use lbf_demo_config_api::demo::config::{Config, Script};

/// Prints the bech32 encoded eq_validator address to stdout
pub fn addresses_eq_validator(config: Config) {
    let Script(raw_script) = config.eq_validator;
    let plutus_script: cardano_serialization_lib::plutus::PlutusScript =
        cardano_serialization_lib::plutus::PlutusScript::new_v2(raw_script);
    let plutus_script_hash = plutus_script.hash();
    let plutus_stake_credential =
        cardano_serialization_lib::address::StakeCredential::from_scripthash(&plutus_script_hash);
    let plutus_enterprise_address = cardano_serialization_lib::address::EnterpriseAddress::new(
        // TODO(jaredponn): October 10, 2024: the CLI should pass in whether its mainnet or testnet
        // We use `1` for now because thats the mainnet (`0` would be the testnet)
        1,
        &plutus_stake_credential,
    );
    let plutus_address = plutus_enterprise_address.to_address();

    let plutus_bech32_address: String = plutus_address.to_bech32(None).unwrap();

    println!("{}", plutus_bech32_address);
}
