{ inputs, ... }:
{
  imports = [
    inputs.pre-commit-hooks.flakeModule
  ];
  perSystem =
    { config, ... }:
    {
      devShells.dev-pre-commit = config.pre-commit.devShell;
      devShells.default = config.pre-commit.devShell;

      pre-commit = {
        settings = {
          excludes = [
            "spago-packages.nix"
          ];

          hooks = {
            nixfmt-rfc-style.enable = true;
            deadnix.enable = true;
            statix.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            # shellcheck.enable = true;
            hlint.enable = true;
            typos = {
              enable = true;
              excludes = [ "fourmolu.yaml" ];
            };
            markdownlint.enable = true;
            dhall-format.enable = true;
            purty.enable = true;
            denolint = {
              enable = true;
              # See NOTE(jaredponn) below
              files = ''^.*\.(ts|tsx)$'';
            };
            denofmt = {
              enable = true;
              # NOTE(jaredponn): We follow the default files this formats, except
              # we exclude markdown, js, and json because those would mess with
              # already existing things.
              #   [1] https://docs.deno.com/runtime/manual/tools/formatter
              files = ''^.*\.(ts|tsx)$'';
            };
          };

          settings = {
            ormolu.cabalDefaultExtensions = true;
            statix.ignore = [ "**spago-packages.nix" ];
            typos.configPath = "./.typos.toml";
          };
        };
      };
    };
}
