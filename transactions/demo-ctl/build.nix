{ inputs, ... }:
{
  perSystem =
    {
      pkgs,
      pkgsForCtl,
      config,
      system,
      inputs',
      self',
      ...
    }:

    let
      dataDir = "data";
      # Adds data/demo-plutarch-config.json and data/demo-plutustx-config.json to dev/build environments
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
      pursFlake = inputs.flake-lang.lib."${system}".purescriptFlake {
        inherit data;
        src = ./.;
        projectName = "demo-ctl";
        strictComp = true;
        packageJson = ./package.json;
        packageLock = ./package-lock.json;

        extraSources = [
          # LB base schema and runtime libs
          inputs'.lbf.packages.lbf-prelude-purescript
          inputs'.lbf.packages.lbf-plutus-purescript
          inputs'.lbf.packages."purescript:lbr-prelude:src" # TODO(bladyjoker): C'mon man make this into lbr-prelude-purescript-src and lbr-plutus-purescript-src
          inputs'.lbf.packages."purescript:lbr-plutus:src"

          # Demo API
          config.packages.lbf-demo-config-api-purescript
          config.packages.lbf-demo-plutus-api-purescript
        ];

        shell = {
          withRuntime = false;
          packageLockOnly = true;
          packages = [
            pkgs.nodejs-18_x
            pkgs.bashInteractive
            pkgs.fd
            pkgsForCtl.ogmios
            pkgsForCtl.kupo
            self'.packages.pc-demo-ctl-tests
          ] ++ config.settings.shell.tools;
          shellHook = config.settings.shell.hook;
        };

      };
    in
    {

      devShells.dev-demo-ctl = pursFlake.devShell;

      inherit (pursFlake) packages;

      checks =
        let
          data-drv = pkgs.linkFarm "data" data;
          nodeModules = "${self'.packages."purescript:demo-ctl:node-modules"}/lib/node_modules";
        in
        {
          "demo-ctl-checks" = pkgs.stdenv.mkDerivation {
            name = "demo-ctl-checks";
            phases = [
              "unpackPhase"
              "checkPhase"
              "buildPhase"
            ];
            unpackPhase = ''
              echo "Linking data"
              ln -s ${./wallets} ./wallets
              ln -s ${data-drv} ./${dataDir}
              ln -s ${nodeModules} node_modules

              cp -r ${self'.packages."purescript:demo-ctl:lib"}/* .
            '';
            checkPhase = ''
              export NODE_PATH="${nodeModules}"
              ${self'.packages.pc-demo-ctl-tests}/bin/pc-demo-ctl-tests -t=false
            '';
            buildPhase = ''
              touch $out
            '';
            doCheck = true;
          };
        };

      cardano-devnet.initialFunds = {
        "60a5587dc01541d4ad17d7a4416efee274d833f2fc894eef79976a3d06" = 9000000000;
      };

      process-compose.pc-demo-ctl-tests =
        let
          addr = "addr_test1vzj4slwqz4qaftgh67jyzmh7uf6dsvljljy5ammeja4r6ps43uflk";
          cardano-cli = "${inputs'.cardano-node.packages.cardano-cli}/bin/cardano-cli";
          jq = "${pkgs.jq}/bin/jq";
          devnetDir = ".devnet";
        in
        {
          settings.processes = {
            tests = {
              command = ''
                ls .
                ${pkgs.nodejs}/bin/node --enable-source-maps -e 'import("./output/Test.Main/index.js").then(m => m.main())'
              '';
              depends_on = {
                fund_wallet.condition = "process_completed_successfully";
                cardano_devnet.condition = "process_healthy";
                ogmios.condition = "process_healthy";
                kupo.condition = "process_healthy";
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
                    --socket-path ${devnetDir}/node.socket \
                    --testnet-magic 42'';
                initial_delay_seconds = 1;
                period_seconds = 1;
              };
            };

            fund_wallet = {
              command = ''
                set -e
                ${cardano-cli} conway transaction build \
                  --tx-in $(${cardano-cli} query utxo --address ${addr} --socket-path ${devnetDir}/node.socket --testnet-magic 42 --output-json | ${jq} -r 'keys[0]') \
                  --change-address ${addr} \
                  --socket-path ${devnetDir}/node.socket \
                  --testnet-magic 42 \
                  --out-file ${devnetDir}/tx.raw

                ${cardano-cli} conway transaction sign \
                  --tx-body-file ${devnetDir}/tx.raw \
                  --signing-key-file ./wallets/test.skey \
                  --out-file ${devnetDir}/tx.signed

                ${cardano-cli} conway transaction submit \
                  --tx-file ${devnetDir}/tx.signed \
                  --socket-path ${devnetDir}/node.socket \
                  --testnet-magic 42
              '';
              depends_on.cardano_devnet.condition = "process_healthy";
            };

            ogmios = {
              command = ''
                ${pkgsForCtl.ogmios}/bin/ogmios \
                  --node-socket ${devnetDir}/node.socket \
                  --node-config ${devnetDir}/config.json
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

            kupo = {
              command = ''
                ${pkgsForCtl.kupo}/bin/kupo \
                  --node-socket ${devnetDir}/node.socket \
                  --node-config ${devnetDir}/config.json \
                  --in-memory \
                  --match '*' \
                  --since origin
              '';
              readiness_probe = {
                http_get = {
                  host = "127.0.0.1";
                  port = 1442;
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
