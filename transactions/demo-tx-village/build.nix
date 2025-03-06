# DESCRIPTION:
# - A Rust demo project which provides an interface to tx-village suitable for
# interacting with the demo project
# In particular, this provides the following functionality to interact with the
# demo project
#   - Building and submitting transactions which may use the EqValidator script
#   - Querying UTxOs by human readable bech32 addresses and optionally filtering
#     them by datum
#   - Querying the human readable bech32 address of the EqValidator script
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
      rustFlake = inputs.flake-lang.lib."${system}".rustFlake {
        inherit data;
        src = ./.;
        crateName = "demo-tx-village";
        exportTests = true;
        nativeBuildInputs =
          (pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.pkg-config
          ])
          ++ (pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.gcc
            pkgs.darwin.apple_sdk.frameworks.Security
            pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
          ]);
        buildInputs = [
          pkgs.openssl.dev
        ];

        extraSources = [
          # LB base schema and runtime libs
          inputs'.lbf.packages.lbf-prelude-rust
          inputs'.lbf.packages.lbf-plutus-rust
          inputs'.lbf.packages.lbr-prelude-rust-src
          inputs'.lbf.packages.lbr-prelude-derive-rust-src

          # Demo API
          config.packages.lbf-demo-config-api-rust
          config.packages.lbf-demo-plutus-api-rust
        ];

        devShellTools = [
          config.packages.demo-haskell-cli
          self'.packages.demo-tx-village-rust
          self'.packages.pc-demo-tx-village-tests
        ];

        devShellHook =
          config.settings.shell.hook
          + ''
            echo "LambdaBuffers for Rust testsuite"
            echo ""
            echo "Run pc-demo-tx-village-tests to execute the testsuite."
            echo "or pc-demo-tx-village-tests up ogmios cardano_devnet to spin up an environment"
            echo ""
          '';

      };
    in
    {

      inherit (rustFlake) packages devShells;

      checks =
        let
          data-drv = pkgs.linkFarm "data" data;
        in
        {
          "demo-tx-village-checks" = pkgs.stdenv.mkDerivation {
            name = "demo-tx-village-checks";
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
              pc-demo-tx-village-tests -t=false
            '';
            buildPhase = ''
              mkdir $out
            '';
            buildInputs = [
              self'.packages.pc-demo-tx-village-tests
              self'.packages.demo-tx-village-rust
              config.packages.demo-haskell-cli
            ];
            doCheck = true;
          };
        };

      cardano-devnet.initialFunds = {
        "60a5587dc01541d4ad17d7a4416efee274d833f2fc894eef79976a3d06" = 9000000000;
      };

      process-compose.pc-demo-tx-village-tests = {
        settings.processes = {
          tests = {
            command = "${self'.packages.demo-tx-village-rust-test}/bin/run_tests.sh";
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
