{ inputs, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule
  ];
  perSystem = { config, ... }:
    {
      devShells.dev-pre-commit = config.pre-commit.devShell;
      devShells.default = config.pre-commit.devShell;

      pre-commit = {
        settings = {
          excludes = [
            "transactions/ctl-demo/spago-packages.nix"
          ];

          hooks = {
            nixpkgs-fmt.enable = true;
            deadnix.enable = true;
            statix.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            shellcheck.enable = true;
            hlint.enable = true;
            typos.enable = true;
            markdownlint.enable = true;
            dhall-format.enable = true;
            purty.enable = true;
          };

          settings = {
            ormolu.cabalDefaultExtensions = true;
            statix.ignore = [ "**spago-packages.nix" ];
          };
        };
      };
    };
}
