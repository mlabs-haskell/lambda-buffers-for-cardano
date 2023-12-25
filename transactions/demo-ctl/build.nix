{ inputs, ... }:
{
  perSystem = { pkgs, pkgsForCtl, config, system, inputs', ... }:

    let
      pursFlake = inputs.lbf.lib.${system}.purescriptFlake {
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

        shell = {
          withRuntime = false;
          packageLockOnly = true;
          packages =
            [
              pkgs.nodejs-18_x
              pkgs.bashInteractive
              pkgs.fd
              pkgsForCtl.plutip-server
              pkgsForCtl.ogmios
              pkgsForCtl.kupo
            ] ++ config.settings.shell.tools;
          shellHook = config.settings.shell.hook;
        };

      };
    in
    {

      devShells.dev-demo-ctl = pursFlake.devShell;

      inherit (pursFlake) packages checks;

    };
}
