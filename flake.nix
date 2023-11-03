{
  description = "LambdaBuffers Cardano Demo";
  inputs = {
    lbf.url = "github:mlabs-haskell/lambda-buffers?ref=bladyjoker/plutarch";
    haskell-nix.follows = "lbf/haskell-nix";
    nixpkgs.follows = "lbf/nixpkgs";
    flake-utils.follows = "lbf/flake-utils";
    pre-commit-hooks.follows = "lbf/pre-commit-hooks";
    mlabs-tooling.follows = "lbf/mlabs-tooling";
    hci-effects.follows = "lbf/hci-effects";
    ctl.follows = "lbf/ctl";
    iohk-nix.follows = "lbf/iohk-nix";
    flake-parts.follows = "lbf/flake-parts";
    plutarch.follows = "lbf/plutarch";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./pkgs.nix
        ./settings.nix
        ./pre-commit.nix
        ./hercules-ci.nix
      ];
      debug = true;
      systems = [ "x86_64-linux" "x86_64-darwin" ];
    };
}
