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

        name = "demo-plutarch";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          # LB base schema and runtimes libs
          # Plutarch
          "${inputs'.lbf.packages.lbf-prelude-plutarch}"
          "${inputs'.lbf.packages.lbf-plutus-plutarch}"
          "${inputs'.lbf.packages.lbr-plutarch-src}"
          # Prelude
          "${inputs'.lbf.packages.lbf-prelude-haskell}"
          "${inputs'.lbf.packages.lbr-prelude-haskell-src}"

          # Plutarch itself
          "${inputs.plutarch}"
          "${inputs.plutarch}/plutarch-ledger-api"

          # Demo API
          "${config.packages.lbf-demo-plutus-api-plutarch}"
          "${config.packages.lbf-demo-config-api-haskell}"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in
    {
      devShells.dev-demo-plutarch = hsFlake.devShell;

      packages = {
        # WARN(bladyjoker): We have to pick the hsFlake.packages like this otherwise flake-parts goes into `infinite recursion`.
        demo-plutarch-lib = hsFlake.packages."demo-plutarch:lib:demo-plutarch";

        demo-plutarch-cli = hsFlake.packages."demo-plutarch:exe:demo-plutarch-cli";

        demo-plutarch-config = pkgs.stdenv.mkDerivation {
          name = "demo-plutarch-config";
          src = ./.;
          buildPhase = ''${config.packages.demo-plutarch-cli}/bin/demo-plutarch-cli compile'';
          installPhase = "cp demo-config.json $out";
        };
      };

      inherit (hsFlake) checks;
    };
}
