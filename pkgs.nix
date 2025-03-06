# Repo-wide Nixpkgs with a ton of overlays
{ inputs, ... }:
{
  perSystem =
    { pkgs, system, ... }:
    {

      _module.args = {

        pkgs = import inputs.nixpkgs {
          inherit system;
        };

        # TODO(bladyjoker): If we use recent nixpkgs we get: `error: nodejs_14 has been removed as it is EOL`. That's why we use CTL's old nixpkgs.
        pkgsForCtl = import inputs.ctl.inputs.nixpkgs {
          inherit system;
          inherit (inputs.haskell-nix) config;
          overlays = [
            inputs.haskell-nix.overlay
            inputs.iohk-nix.overlays.crypto
            inputs.ctl.overlays.runtime
            inputs.ctl.overlays.purescript
            inputs.ctl.overlays.spago
          ];
        };
      };
    };
}
