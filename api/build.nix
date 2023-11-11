{ inputs, ... }: {
  perSystem = { system, ... }:
    {
      packages.lbf-demo-plutus-api-haskell = inputs.lbf.lib."${system}".lbfPlutusHaskell {
        name = "lbf-demo-plutus-api";
        src = ./.;
        files = [ "Demo/Plutus.lbf" ];
      };

      packages.lbf-demo-plutus-api-purescript = inputs.lbf.lib."${system}".lbfPlutusPurescript {
        name = "lbf-demo-plutus-api";
        src = ./.;
        files = [ "Demo/Plutus.lbf" ];
      };

      packages.lbf-demo-plutus-api-plutarch = inputs.lbf.lib."${system}".lbfPlutarch {
        name = "lbf-demo-plutus-api-plutarch";
        src = ./.;
        files = [ "Demo/Plutus.lbf" ];
      };

      packages.lbf-demo-config-api-haskell = inputs.lbf.lib."${system}".lbfPreludeHaskell {
        name = "lbf-demo-config-api";
        src = ./.;
        files = [ "Demo/Config.lbf" ];
      };

      packages.lbf-demo-config-api-purescript = inputs.lbf.lib."${system}".lbfPreludePurescript {
        name = "lbf-demo-config-api";
        src = ./.;
        files = [ "Demo/Config.lbf" ];
      };

    };
}

