{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { dev = false; fixity-th = true; };
    package = {
      specVersion = "2.4";
      identifier = { name = "fourmolu"; version = "0.10.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Matt Parsons <parsonsmatt@gmail.com>\nGeorge Thomas <georgefsthomas@gmail.com>\nBrandon Chinn <brandonchinn178@gmail.com>";
      author = "";
      homepage = "https://github.com/fourmolu/fourmolu";
      url = "";
      synopsis = "A formatter for Haskell source code";
      description = "A formatter for Haskell source code.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE.md" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "data/**/*.hs"
        "data/**/*.txt"
        "data/**/*.cabal"
        "extract-hackage-info/hackage-info.json"
        "fixity-tests/*.hs"
        "region-tests/*.hs"
        "fourmolu.yaml"
        ];
      extraTmpFiles = [];
      extraDocFiles = [ "CHANGELOG.md" "README.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
          (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
          (hsPkgs."MemoTrie" or (errorHandler.buildDepError "MemoTrie"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."ghc-lib-parser" or (errorHandler.buildDepError "ghc-lib-parser"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (pkgs.lib).optional (!flags.fixity-th) (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"));
        buildable = true;
        modules = [
          "Ormolu/Config/TH"
          "Ormolu/Config/Types"
          "GHC/DynFlags"
          "Ormolu"
          "Ormolu/Config"
          "Ormolu/Diff/ParseResult"
          "Ormolu/Diff/Text"
          "Ormolu/Exception"
          "Ormolu/Imports"
          "Ormolu/Parser"
          "Ormolu/Parser/CommentStream"
          "Ormolu/Parser/Pragma"
          "Ormolu/Parser/Result"
          "Ormolu/Printer"
          "Ormolu/Printer/Combinators"
          "Ormolu/Printer/Comments"
          "Ormolu/Printer/Internal"
          "Ormolu/Printer/Meat/Common"
          "Ormolu/Printer/Meat/Declaration"
          "Ormolu/Printer/Meat/Declaration/Annotation"
          "Ormolu/Printer/Meat/Declaration/Class"
          "Ormolu/Printer/Meat/Declaration/Data"
          "Ormolu/Printer/Meat/Declaration/Default"
          "Ormolu/Printer/Meat/Declaration/Foreign"
          "Ormolu/Printer/Meat/Declaration/Instance"
          "Ormolu/Printer/Meat/Declaration/RoleAnnotation"
          "Ormolu/Printer/Meat/Declaration/Rule"
          "Ormolu/Printer/Meat/Declaration/Signature"
          "Ormolu/Printer/Meat/Declaration/Splice"
          "Ormolu/Printer/Meat/Declaration/Type"
          "Ormolu/Printer/Meat/Declaration/TypeFamily"
          "Ormolu/Printer/Meat/Declaration/Value"
          "Ormolu/Printer/Meat/Declaration/OpTree"
          "Ormolu/Printer/Meat/Declaration/Warning"
          "Ormolu/Printer/Meat/ImportExport"
          "Ormolu/Printer/Meat/Module"
          "Ormolu/Printer/Meat/Pragma"
          "Ormolu/Printer/Meat/Type"
          "Ormolu/Printer/Operators"
          "Ormolu/Fixity"
          "Ormolu/Fixity/Internal"
          "Ormolu/Fixity/Parser"
          "Ormolu/Fixity/Printer"
          "Ormolu/Printer/SpanStream"
          "Ormolu/Processing/Common"
          "Ormolu/Processing/Cpp"
          "Ormolu/Processing/Preprocess"
          "Ormolu/Terminal"
          "Ormolu/Utils"
          "Ormolu/Utils/Cabal"
          "Ormolu/Utils/Fixity"
          "Ormolu/Utils/IO"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "fourmolu" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc-lib-parser" or (errorHandler.buildDepError "ghc-lib-parser"))
            (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."fourmolu" or (errorHandler.buildDepError "fourmolu"))
            ];
          buildable = true;
          modules = [ "Paths_fourmolu" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ] ++ [ "" ];
          };
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc-lib-parser" or (errorHandler.buildDepError "ghc-lib-parser"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-megaparsec" or (errorHandler.buildDepError "hspec-megaparsec"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."path" or (errorHandler.buildDepError "path"))
            (hsPkgs."path-io" or (errorHandler.buildDepError "path-io"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."fourmolu" or (errorHandler.buildDepError "fourmolu"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.fourmolu.components.exes.fourmolu or (pkgs.buildPackages.fourmolu or (errorHandler.buildToolDepError "fourmolu:fourmolu")))
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          modules = [
            "IntegrationUtils"
            "Ormolu/CabalInfoSpec"
            "Ormolu/Diff/TextSpec"
            "Ormolu/Fixity/ParserSpec"
            "Ormolu/Fixity/PrinterSpec"
            "Ormolu/HackageInfoSpec"
            "Ormolu/OpTreeSpec"
            "Ormolu/Parser/OptionsSpec"
            "Ormolu/Parser/ParseFailureSpec"
            "Ormolu/Parser/PragmaSpec"
            "Ormolu/PrinterSpec"
            "Ormolu/Config/OptionsSpec"
            "Ormolu/Config/PrinterOptsSpec"
            ];
          hsSourceDirs = [ "tests/utils/" "tests" ];
          mainPath = [ "Spec.hs" ];
          };
        "region-tests" = {
          depends = [
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.fourmolu.components.exes.fourmolu or (pkgs.buildPackages.fourmolu or (errorHandler.buildToolDepError "fourmolu:fourmolu")))
            ];
          buildable = true;
          modules = [ "IntegrationUtils" ];
          hsSourceDirs = [ "tests/utils/" "region-tests" ];
          mainPath = [ "Main.hs" ];
          };
        "fixity-tests" = {
          depends = [
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.fourmolu.components.exes.fourmolu or (pkgs.buildPackages.fourmolu or (errorHandler.buildToolDepError "fourmolu:fourmolu")))
            ];
          buildable = true;
          modules = [ "IntegrationUtils" ];
          hsSourceDirs = [ "tests/utils/" "fixity-tests" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }