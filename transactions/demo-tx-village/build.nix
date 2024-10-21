# DESCRIPTION:
# - A Rust demo project which provides an interface to tx-village suitable for
# interacting with the demo project
# In particular, this provides the following functionality to interact with the
# demo project
#   - Building and submitting transactions which may use the EqValidator script
#   - Querying UTxOs by human readable bech32 addresses and optionally filtering
#     them by datum
#   - Querying the human readable bech32 address of the EqValidator script
{ inputs, ... }: {
  perSystem = { config, inputs', system, pkgs, ... }:

    let
      rustFlake = inputs.flake-lang.lib."${system}".rustFlake
        {
          src = ./.;
          crateName = "demo-tx-village";
          nativeBuildInputs =
            (pkgs.lib.optionals pkgs.stdenv.isLinux [
              pkgs.pkg-config
            ]) ++
            (pkgs.lib.optionals pkgs.stdenv.isDarwin
              [
                pkgs.gcc
                pkgs.darwin.apple_sdk.frameworks.Security
                pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
              ]);
          buildInputs = [
            pkgs.openssl.dev
          ];
          testTools = [
            inputs'.plutip.packages."plutip-core:exe:local-cluster"
            inputs'.ogmios.packages."ogmios:exe:ogmios"
            pkgs.cargo-nextest
            config.packages.demo-haskell-cli
          ];
          cargoNextestExtraArgs = "--test-threads 1";

          extraSources = [
            # LB base schema and runtime libs
            inputs'.tx-village.packages.tx-bakery-rust-src
            inputs'.tx-village.packages.tx-bakery-ogmios-rust-src
            inputs'.tx-village.packages.tx-bakery-plutip-rust-src
            inputs'.lbf.packages.lbf-prelude-rust
            inputs'.lbf.packages.lbf-plutus-rust
            inputs'.lbf.packages.lbr-prelude-rust-src
            inputs'.lbf.packages.lbr-prelude-derive-rust-src

            # Demo API
            config.packages.lbf-demo-config-api-rust
            config.packages.lbf-demo-plutus-api-rust
          ];

          data = [
            {
              name = "demo-plutarch-config.json";
              path = config.packages.demo-plutarch-config;
            }
            {
              name = "demo-plutustx-config.json";
              path = config.packages.demo-plutustx-config;
            }

          ];

          devShellHook = config.settings.shell.hook;
        };
    in
    {

      inherit (rustFlake) packages checks devShells;

    };
}
