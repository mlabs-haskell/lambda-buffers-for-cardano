# DESCRIPTION.
# - TS demo project based off of the PureScript / Rust project which uses
#   cardano-serialization-lib and ogmios.
#   Running the tests requires a JSON *RTS configuration file* -- see
#   `./src/rtsconfig.ts` for details in the environment variable
#   `TEST_RTS_CONFIG`.
#   If this is not provided, the tests will automatically spin everything up
#   for you.
#   
#   Tests may be ran with
#   ```
#   npm run test
#   ```
#   
# - The shell script `demo-rts` can be used to start a test RTS (ogmios +
#   plutip) for you, and it will also provide a RTS configuration file for
#   running the tests.
#
# LIMITATIONS.
# - The version of cardano-serialization-lib that this uses leaks memory
#   everywhere. When one uses `cardano-serialization-lib`, one should compile
#   it themselves (recall `wasm-pack` uses `wasm-bindgen` to compile from Rust
#   to TS) manually with the flag `--weak-refs` for `wasm-pack` to enable
#   garbage collection.
{ inputs, ... }: {
  perSystem = { config, inputs', system, pkgs, ... }:
    let
      demoRts = pkgs.writeShellApplication {
        name = "demo-rts";
        runtimeInputs =
          [ pkgs.jq inputs'.plutip.packages."plutip-core:exe:local-cluster" inputs'.ogmios.packages."ogmios:exe:ogmios" ];
        text = builtins.readFile ./demo-rts.sh;
      };

      tsFlake = inputs.flake-lang.lib."${system}".typescriptFlake
        rec
        {
          name = "demo";
          src = ./.;

          testTools = [
            inputs'.plutip.packages."plutip-core:exe:local-cluster"
            inputs'.ogmios.packages."ogmios:exe:ogmios"
            demoRts
          ];

          npmExtraDependencies = [
            # LB base schema and runtime libs
            inputs'.lbf.packages.lbf-prelude-typescript
            inputs'.lbf.packages.lbf-plutus-typescript

            # Demo API
            config.packages.lbf-demo-config-api-typescript
            config.packages.lbf-demo-plutus-api-typescript
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
          devShellTools = config.settings.shell.tools ++ testTools;
        };
    in
    {
      inherit (tsFlake) checks devShells;
      packages = {
        demo-rts-typescript = demoRts;
      };
    };
}
