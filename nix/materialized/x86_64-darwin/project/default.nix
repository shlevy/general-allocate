{
  pkgs = hackage:
    {
      packages = {
        safe-exceptions.revision = import ./cabal-files/safe-exceptions.nix;
        exceptions.revision = (((hackage.exceptions)."0.10.4").revisions).default;
        mtl.revision = (((hackage.mtl)."2.2.2").revisions).default;
        ghc-bignum.revision = (((hackage.ghc-bignum)."1.2").revisions).default;
        ghc-prim.revision = (((hackage.ghc-prim)."0.8.0").revisions).default;
        unliftio-core.revision = import ./cabal-files/unliftio-core.nix;
        containers.revision = (((hackage.containers)."0.6.5.1").revisions).default;
        stm.revision = (((hackage.stm)."2.5.0.2").revisions).default;
        base.revision = (((hackage.base)."4.16.4.0").revisions).default;
        resourcet.revision = import ./cabal-files/resourcet.nix;
        primitive.revision = import ./cabal-files/primitive.nix;
        deepseq.revision = (((hackage.deepseq)."1.4.6.1").revisions).default;
        rts.revision = (((hackage.rts)."1.0.2").revisions).default;
        template-haskell.revision = (((hackage.template-haskell)."2.18.0.0").revisions).default;
        data-array-byte.revision = import ./cabal-files/data-array-byte.nix;
        transformers.revision = (((hackage.transformers)."0.5.6.2").revisions).default;
        array.revision = (((hackage.array)."0.5.4.0").revisions).default;
        ghc-boot-th.revision = (((hackage.ghc-boot-th)."9.2.6").revisions).default;
        pretty.revision = (((hackage.pretty)."1.1.3.6").revisions).default;
        };
      compiler = {
        version = "9.2.6";
        nix-name = "ghc926";
        packages = {
          "pretty" = "1.1.3.6";
          "array" = "0.5.4.0";
          "mtl" = "2.2.2";
          "stm" = "2.5.0.2";
          "ghc-prim" = "0.8.0";
          "ghc-boot-th" = "9.2.6";
          "base" = "4.16.4.0";
          "ghc-bignum" = "1.2";
          "exceptions" = "0.10.4";
          "rts" = "1.0.2";
          "transformers" = "0.5.6.2";
          "template-haskell" = "2.18.0.0";
          "deepseq" = "1.4.6.1";
          "containers" = "0.6.5.1";
          };
        };
      };
  extras = hackage:
    { packages = { general-allocate = ./.plan.nix/general-allocate.nix; }; };
  modules = [
    ({ lib, ... }:
      { packages = { "general-allocate" = { flags = {}; }; }; })
    ({ lib, ... }:
      {
        packages = {
          "resourcet".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "safe-exceptions".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "unliftio-core".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "general-allocate".components.library.planned = lib.mkOverride 900 true;
          "data-array-byte".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "primitive".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }