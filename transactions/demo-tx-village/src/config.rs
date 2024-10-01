use lbf_demo_config_api::demo::config::Config;

static DEMO_CONFIG_ENV_VAR_KEY: &'static str = "DEMO_CONFIG";

/// Gets the demo protocol config from the filepath stored in the environment variable
/// `DEMO_CONFIG`
pub async fn get_config() -> Config {
    match std::env::var(DEMO_CONFIG_ENV_VAR_KEY) {
        Ok(path) => {
            let conf_str = tokio::fs::read_to_string(&path).await.expect(&format!(
                    "Expected environment variable `DEMO_CONFIG` to contain a filepath to a LB JSON `Config` object, but file reading failed with path {}", &path)
                );

            let conf: Config = lbr_prelude::json::Json::from_json_string(&conf_str) .expect(&format!(
                    "Expected environment variable `DEMO_CONFIG` to contain a filepath to a LB JSON `Config` object, but failed to deserialize the JSON object at path {}", &path)
                    );
            return conf;
        }
        Err(err) => {
            panic!("Expected environment variable `DEMO_CONFIG` to contain a filepath to a LB JSON `Config` object, but environment variable lookup failed: {}", err)
        }
    }
}
