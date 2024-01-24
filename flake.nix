{
  description = "LambdaBuffers Cardano Demo";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    # Flakes as modules, using this extensively to organize the repo into modules (build.nix files)
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Hercules CI effects
    hci-effects.url = "github:hercules-ci/hercules-ci-effects";

    # Code quality automation
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    lbf.url = "github:mlabs-haskell/lambda-buffers";
    flake-lang.url = "github:mlabs-haskell/flake-lang.nix/26e94d19b449c9f944647eaa8c4ebf05209b00ce";
    haskell-nix.follows = "lbf/haskell-nix";
    iohk-nix.follows = "lbf/iohk-nix";
    ctl.follows = "lbf/ctl";
    plutarch.follows = "lbf/plutarch";
    crane.url = "github:ipetkov/crane";

    plutip.url = "github:mlabs-haskell/plutip/1bf0b547cd3689c727586abb8385c008fb2a3d1c";
    ogmios.url = "github:mlabs-haskell/ogmios-nixos/78e829e9ebd50c5891024dcd1004c2ac51facd80";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./pkgs.nix
        ./settings.nix
        ./pre-commit.nix
        ./hercules-ci.nix
        ./api/build.nix
        ./validation/demo-plutarch/build.nix
        ./validation/demo-plutustx/build.nix
        ./transactions/demo-ctl/build.nix
        ./transactions/demo-rust/build.nix
      ];
      debug = true;
      systems = [ "x86_64-linux" ]; # TODO(szg251): fix darwin CI build and add "x86_64-darwin"

    };

}
