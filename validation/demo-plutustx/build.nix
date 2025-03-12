{ inputs, ... }:
{
  perSystem =
    {
      config,
      system,
      pkgs,
      inputs',
      ...
    }:
    let
      hsFlake = inputs.flake-lang.lib."${system}".haskellPlutusFlake {
        src = ./.;

        name = "demo-plutustx";

        inherit (config.settings.haskell) compiler-nix-name;
        # Using plutus-tx 1.40.0
        index-state = "2025-01-29T08:56:13Z";

        dependencies = [
          # LB base schema and runtimes libs
          # PlutusTx
          "${inputs'.lbf.packages.lbf-plutus-plutustx}"
          "${inputs'.lbf.packages.lbr-plutustx-src}"
          # Prelude
          "${inputs'.lbf.packages.lbf-prelude-haskell}"
          "${inputs'.lbf.packages.lbf-prelude-plutustx}"
          "${inputs'.lbf.packages.lbr-prelude-haskell-src}"

          # Demo API
          "${config.packages.lbf-demo-config-api-haskell}"
          "${config.packages.lbf-demo-plutus-api-plutustx}"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in
    {
      devShells.dev-demo-plutustx = hsFlake.devShell;

      packages = {
        # WARN(bladyjoker): We have to pick the hsFlake.packages like this otherwise flake-parts goes into `infinite recursion`.
        demo-plutustx-lib = hsFlake.packages."demo-plutustx:lib:demo-plutustx";

        demo-plutustx-cli = hsFlake.packages."demo-plutustx:exe:demo-plutustx-cli";

        demo-plutustx-config = pkgs.stdenv.mkDerivation {
          name = "demo-plutustx-config";
          src = ./.;
          buildPhase = ''${config.packages.demo-plutustx-cli}/bin/demo-plutustx-cli compile'';
          installPhase = "cp demo-config.json $out";
        };
      };

      inherit (hsFlake) checks;
    };
}
