{ inputs, ... }: {
  perSystem = { config, inputs', system, pkgs, ... }:

    let
      rustFlake = inputs.flake-lang.lib."${system}".rustFlake
        {
          src = ./.;
          inherit (inputs) crane;
          crateName = "demo";
          nativeBuildInputs =
            pkgs.lib.optionals pkgs.stdenv.isDarwin
              [
                pkgs.darwin.apple_sdk.frameworks.Security
                pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
              ];
          devShellTools = [
            inputs'.plutip.packages."plutip-core:exe:local-cluster"
            inputs'.ogmios.packages."ogmios:exe:ogmios"
          ];

          extraSources = [
            # LB base schema and runtime libs
            {
              name = "lbf-prelude";
              path = inputs'.lbf.packages.lbf-prelude-rust;
            }
            {
              name = "lbf-plutus";
              path = inputs'.lbf.packages.lbf-plutus-rust;
            }
            {
              name = "lbr-prelude";
              path = inputs'.lbf.packages.lbr-prelude-rust-src;
            }
            {
              name = "lbr-prelude-derive";
              path = inputs'.lbf.packages.lbr-prelude-derive-rust-src;
            }

            # Demo API
            {
              name = "lbf-demo-config-api";
              path = config.packages.lbf-demo-config-api-rust;
            }
            {
              name = "lbf-demo-plutus-api";
              path = config.packages.lbf-demo-plutus-api-rust;
            }
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