{ inputs, ... }:
{
  perSystem =
    { system, ... }:
    {
      packages = {
        lbf-demo-plutus-api-haskell = inputs.lbf.lib."${system}".lbfPlutusHaskell {
          name = "lbf-demo-plutus-api";
          src = ./.;
          files = [
            "Demo/Plutus.lbf"
            "Demo/Request.lbf"
            "Demo/Response.lbf"
          ];
        };

        lbf-demo-plutus-api-plutustx = inputs.lbf.lib."${system}".lbfPlutusTx {
          name = "lbf-demo-plutus-api-plutustx";
          src = ./.;
          files = [ "Demo/Plutus.lbf" ];
        };

        lbf-demo-plutus-api-purescript = inputs.lbf.lib."${system}".lbfPlutusPurescript {
          name = "lbf-demo-plutus-api";
          src = ./.;
          files = [ "Demo/Plutus.lbf" ];
        };

        lbf-demo-plutus-api-plutarch = inputs.lbf.lib."${system}".lbfPlutarch {
          name = "lbf-demo-plutus-api-plutarch";
          src = ./.;
          files = [ "Demo/Plutus.lbf" ];
        };

        lbf-demo-plutus-api-rust = inputs.lbf.lib."${system}".lbfPlutusRust {
          name = "lbf-demo-plutus-api";
          src = ./.;
          files = [
            "Demo/Plutus.lbf"
            "Demo/Request.lbf"
            "Demo/Response.lbf"
          ];
        };

        lbf-demo-plutus-api-typescript = inputs.lbf.lib."${system}".lbfPlutusTypescript {
          name = "lbf-demo-plutus-api";
          src = ./.;
          files = [ "Demo/Plutus.lbf" ];
        };

        lbf-demo-config-api-haskell = inputs.lbf.lib."${system}".lbfPreludeHaskell {
          name = "lbf-demo-config-api";
          src = ./.;
          files = [ "Demo/Config.lbf" ];
        };

        lbf-demo-config-api-purescript = inputs.lbf.lib."${system}".lbfPreludePurescript {
          name = "lbf-demo-config-api";
          src = ./.;
          files = [ "Demo/Config.lbf" ];
        };

        lbf-demo-config-api-rust = inputs.lbf.lib."${system}".lbfPreludeRust {
          name = "lbf-demo-config-api";
          src = ./.;
          files = [ "Demo/Config.lbf" ];
        };

        lbf-demo-config-api-typescript = inputs.lbf.lib."${system}".lbfPreludeTypescript {
          name = "lbf-demo-config-api";
          src = ./.;
          files = [ "Demo/Config.lbf" ];
        };
      };
    };
}
