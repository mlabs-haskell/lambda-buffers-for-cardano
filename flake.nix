{
  description = "LambdaBuffers Cardano Demo";
  inputs = {
    # NOTE(bladyjoker): Trying to reuse as many inputs from lbf and flake-lang as possible to reduce the flake.lock size which impacts performance significantly.
    nixpkgs.follows = "lbf/nixpkgs";

    # LambdaBuffers for sharing types
    lbf.url = "github:mlabs-haskell/lambda-buffers/szg251/expand-c13-report";

    # flake-lang.nix for monorepo setup
    flake-lang.follows = "lbf/flake-lang";

    # flake-parts for Flake modules
    flake-parts.follows = "lbf/flake-parts";

    # Hercules CI effects
    hci-effects.follows = "lbf/hci-effects";

    # Code quality automation
    pre-commit-hooks.follows = "lbf/pre-commit-hooks";

    # Cardano transaction library
    ctl.follows = "lbf/ctl";
    haskell-nix.follows = "lbf/flake-lang/haskell-nix";
    iohk-nix.follows = "lbf/flake-lang/iohk-nix";

    # Plutarch eDSL
    plutarch.follows = "lbf/plutarch";

    tx-village = {
      url = "github:mlabs-haskell/tx-village/szg251/v3";
      inputs.lbf.follows = "lbf";
    };

    ogmios.url = "github:mlabs-haskell/ogmios-nix/v6.11.2";

    cardano-devnet-flake.url = "github:mlabs-haskell/cardano-devnet-flake";
    cardano-node.follows = "cardano-devnet-flake/cardano-node";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.process-compose-flake.flakeModule
        inputs.cardano-devnet-flake.flakeModule
        ./pkgs.nix
        ./settings.nix
        ./pre-commit.nix
        ./hercules-ci.nix
        ./api/build.nix
        ./validation/demo-plutarch/build.nix
        ./validation/demo-plutustx/build.nix
        ./transactions/demo-ctl/build.nix
        ./transactions/demo-rust/build.nix
        ./transactions/demo-haskell/build.nix
        ./transactions/demo-typescript/build.nix
        ./transactions/demo-tx-village/build.nix
      ];
      debug = true;
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    };

}
