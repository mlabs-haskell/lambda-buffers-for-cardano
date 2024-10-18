use lbf_demo_config_api::demo::config::Config;

static DEMO_CONFIG_ENV_VAR_KEY: &str = "DEMO_CONFIG";

/// Gets the demo protocol config from the filepath stored in the environment variable
/// `DEMO_CONFIG`
pub async fn get_config() -> Config {
    let path = std::env::var(DEMO_CONFIG_ENV_VAR_KEY).unwrap_or_else(|err| panic!("Expected environment variable `DEMO_CONFIG` to contain a filepath to a LB JSON `Config` object, but environment variable lookup failed: {}", err));

    let conf_str = tokio::fs::read_to_string(&path).await.unwrap_or_else( |err| panic!(
            "Expected environment variable `DEMO_CONFIG` to contain a filepath to a LB JSON `Config` object, but file reading failed with path {} with error {}", &path, err)
        );

    let conf: Config = lbr_prelude::json::Json::from_json_string(&conf_str) .unwrap_or_else(|err| panic!(
            "Expected environment variable `DEMO_CONFIG` to contain a filepath to a LB JSON `Config` object, but failed to deserialize the JSON object at path {} with error {}", &path, err)
            );
    conf
}
