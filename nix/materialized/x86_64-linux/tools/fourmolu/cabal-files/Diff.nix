{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "Diff"; version = "0.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "s.clover@gmail.com";
      author = "Sterling Clover";
      homepage = "";
      url = "";
      synopsis = "O(ND) diff algorithm in haskell.";
      description = "Implementation of the standard diff algorithm, and utilities for pretty printing.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          ];
        buildable = true;
        };
      tests = {
        "diff-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/Diff-0.4.1.tar.gz";
      sha256 = "fd5e4aaac7219bcbb14834fb8580ebe0aae905958d0ad74f5338ea290b372670";
      });
    }) // {
    package-description-override = "name:                Diff\nversion:             0.4.1\nsynopsis:            O(ND) diff algorithm in haskell.\ndescription:         Implementation of the standard diff algorithm, and utilities for pretty printing.\ncategory:            Algorithms\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Sterling Clover\nmaintainer:          s.clover@gmail.com\nTested-With:         GHC == 7.8.4\nBuild-Type:          Simple\nCabal-Version:       >= 1.10\n\nlibrary\n  default-language: Haskell2010\n  build-depends:   base >= 3 && <= 6, array, pretty >= 1.1\n  hs-source-dirs:  src\n  exposed-modules:\n                   Data.Algorithm.Diff,\n                   Data.Algorithm.DiffOutput\n                   Data.Algorithm.DiffContext\n  ghc-options:     -O2 -Wall -funbox-strict-fields\n\nsource-repository head\n  type:      git\n  location:  http://github.com/seereason/Diff\n\ntest-suite diff-tests\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test, src\n  main-is: Test.hs\n  build-depends: base >= 3 && <= 6, array\n                , pretty, QuickCheck, test-framework\n                , test-framework-quickcheck2, process\n                , directory\n  other-modules:\n                   Data.Algorithm.Diff,\n                   Data.Algorithm.DiffOutput\n                   Data.Algorithm.DiffContext\n";
    }