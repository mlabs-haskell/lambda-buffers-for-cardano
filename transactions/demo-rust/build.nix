{ inputs, ... }: {
  perSystem = { config, inputs', system, pkgs, ... }:

    let
      rustFlake = inputs.flake-lang.lib."${system}".rustFlake
        {
          src = ./.;
          crateName = "demo";
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
