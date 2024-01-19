use cardano_serialization_lib::crypto::Ed25519KeyHash;
use cardano_serialization_lib::plutus::PlutusScript;
use data_encoding::HEXLOWER;
use lbf_demo_config_api::demo::config::{Config, Script};
use lbr_prelude::json::Json;
use std::fs;

pub mod ogmios;
pub mod plutip;

pub(crate) fn read_script(path: &str) -> PlutusScript {
    let conf_str = fs::read_to_string(path).expect(&format!(
        "Couldn't read plutarch config JSON file at {}.",
        path
    ));

    let conf: Config = Json::from_json_string(&conf_str)
        .expect(&format!("Couldn't deserialize JSON data of file {}", path));

    let Script(raw_script) = conf.eq_validator;

    let mut serializer = cbor_event::se::Serializer::new_vec();
    serializer.write_bytes(raw_script).unwrap();
    let script = serializer.finalize();

    PlutusScript::from_bytes_v2(script).expect(&format!(
        "Couldn't deserialize PlutusScript of file {}.",
        path
    ))
}

pub(crate) fn parse_pkh(hex_str: &str) -> Ed25519KeyHash {
    Ed25519KeyHash::from_bytes(HEXLOWER.decode(&hex_str.to_owned().into_bytes()).unwrap()).unwrap()
}
