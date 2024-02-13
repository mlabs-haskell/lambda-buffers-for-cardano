{ inputs, ... }: {
  perSystem = { config, inputs', system, ... }:
    let
      # demoRts = pkgs.writeShellApplication {
      #   name = "demo-rts";
      #   runtimeInputs = [ ];
      # };

      tsFlake = inputs.flake-lang.lib."${system}".typescriptFlake
        rec
        {
          name = "demo";
          src = ./.;

          testTools = [
            inputs'.plutip.packages."plutip-core:exe:local-cluster"
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
    };
}
