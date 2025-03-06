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
{ inputs, ... }:
{
  perSystem =
    {
      config,
      inputs',
      system,
      pkgs,
      self',
      ...
    }:
    let
      dataDir = "data";
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

      tsFlake = inputs.flake-lang.lib."${system}".typescriptFlake {
        inherit data;
        name = "demo";
        src = ./.;

        testTools = [
          inputs'.ogmios.packages."ogmios:exe:ogmios"
        ];

        npmExtraDependencies = [
          # LB base schema and runtime libs
          inputs'.lbf.packages.lbf-prelude-typescript
          inputs'.lbf.packages.lbf-plutus-typescript

          # Demo API
          config.packages.lbf-demo-config-api-typescript
          config.packages.lbf-demo-plutus-api-typescript
        ];

        devShellHook = config.settings.shell.hook;
        devShellTools = config.settings.shell.tools ++ [
          self'.packages.pc-demo-typescript-tests
        ];
      };
    in
    {
      inherit (tsFlake) devShells packages;

      checks =
        let
          data-drv = pkgs.linkFarm "data" data;
        in
        {
          "demo-typescript-checks" = pkgs.stdenv.mkDerivation {
            name = "demo-typescript-checks";
            phases = [
              "unpackPhase"
              "checkPhase"
              "buildPhase"
            ];
            unpackPhase = ''
              echo "Linking data"
              ln -s ${./wallets} ./wallets
              ln -s ${data-drv} ./${dataDir}
            '';
            checkPhase = ''
              ${self'.packages.pc-demo-typescript-tests}/bin/pc-demo-typescript-tests -t=false
            '';
            buildPhase = ''
              mkdir $out
            '';
            doCheck = true;
          };
        };

      cardano-devnet.initialFunds = {
        "60a5587dc01541d4ad17d7a4416efee274d833f2fc894eef79976a3d06" = 9000000000;
      };

      process-compose.pc-demo-typescript-tests = {
        settings.processes = {
          tests = {
            command = "${pkgs.nodejs}/bin/node ${self'.packages.demo-typescript}/lib/node_modules/demo-typescript/dist/tests/demo-test.js";
            depends_on = {
              cardano_devnet.condition = "process_healthy";
              ogmios.condition = "process_healthy";
            };
            availability = {
              exit_on_end = true;
              exit_on_skipped = true;
            };
          };

          cardano_devnet = {
            command = config.packages.cardano-devnet;
            readiness_probe = {
              exec.command = ''
                ${inputs'.cardano-node.packages.cardano-cli}/bin/cardano-cli query tip \
                              --socket-path .devnet/node.socket \
                              --testnet-magic 42'';
              initial_delay_seconds = 1;
              period_seconds = 1;
            };
          };

          ogmios = {
            command = ''
              ${inputs'.ogmios.packages."ogmios:exe:ogmios"}/bin/ogmios \
                          --node-socket .devnet/node.socket \
                          --node-config .devnet/config.json
            '';
            readiness_probe = {
              http_get = {
                host = "127.0.0.1";
                port = 1337;
                path = "/health";
              };
              initial_delay_seconds = 2;
              period_seconds = 2;
            };
            depends_on.cardano_devnet.condition = "process_healthy";
          };

        };
      };
    };
}
