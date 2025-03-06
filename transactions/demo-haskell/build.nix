# DESCRIPTION:
# - A Haskell demo project which provides a CLI interface for building TxInfos
# suitable for tx-village to bake and submit such a transaction
{ inputs, ... }:
{
  perSystem =
    { config, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellPlutusFlake {
        src = ./.;

        name = "demo-haskell";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          # LB base schema and runtimes libs
          "${inputs.lbf.packages.${system}.lbf-prelude-haskell}"
          "${inputs.lbf.packages.${system}.lbr-prelude-haskell-src}"

          "${inputs.lbf.packages.${system}.lbr-plutus-haskell-src}"
          "${inputs.lbf.packages.${system}.lbf-plutus-haskell}"

          # Demo API
          "${config.packages.lbf-demo-plutus-api-haskell}"
          "${config.packages.lbf-demo-config-api-haskell}"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in

    {
      devShells.dev-demo-haskell = hsFlake.devShell;

      packages = {
        demo-haskell-cli = hsFlake.packages."demo-haskell:exe:demo-haskell";
      };

      checks.check-demo-haskell = hsFlake.checks."demo-haskell:test:demo-haskell-test";
    };
}
