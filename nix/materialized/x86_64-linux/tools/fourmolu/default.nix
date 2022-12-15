{
  pkgs = hackage:
    {
      packages = {
        "happy".revision = (((hackage."happy")."1.20.0").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.19.1").revisions).default;
        "unordered-containers".flags.debug = false;
        "Diff".revision = (((hackage."Diff")."0.4.1").revisions).default;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3.1").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "dlist".revision = (((hackage."dlist")."1.0").revisions).default;
        "dlist".flags.werror = false;
        "text".revision = (((hackage."text")."1.2.5.0").revisions).default;
        "ghc-lib-parser".revision = (((hackage."ghc-lib-parser")."9.4.3.20221104").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "parser-combinators".revision = (((hackage."parser-combinators")."1.3.0").revisions).default;
        "parser-combinators".flags.dev = false;
        "vector".revision = (((hackage."vector")."0.13.0.0").revisions).default;
        "vector".flags.internalchecks = false;
        "vector".flags.wall = false;
        "vector".flags.boundschecks = true;
        "vector".flags.unsafechecks = false;
        "generically".revision = (((hackage."generically")."0.1").revisions).default;
        "comonad".revision = (((hackage."comonad")."5.0.8").revisions).default;
        "comonad".flags.containers = true;
        "comonad".flags.distributive = true;
        "comonad".flags.indexed-traversable = true;
        "base-compat".revision = (((hackage."base-compat")."0.12.2").revisions).default;
        "bitvec".revision = (((hackage."bitvec")."1.1.3.0").revisions).default;
        "bitvec".flags.libgmp = false;
        "contravariant".revision = (((hackage."contravariant")."1.5.5").revisions).default;
        "contravariant".flags.tagged = true;
        "contravariant".flags.semigroups = true;
        "contravariant".flags.statevar = true;
        "Cabal-syntax".revision = (((hackage."Cabal-syntax")."3.8.1.0").revisions).default;
        "base-compat-batteries".revision = (((hackage."base-compat-batteries")."0.12.2").revisions).default;
        "yaml".revision = (((hackage."yaml")."0.11.8.0").revisions).default;
        "yaml".flags.no-examples = true;
        "yaml".flags.no-exe = true;
        "th-lift-instances".revision = (((hackage."th-lift-instances")."0.1.20").revisions).default;
        "assoc".revision = (((hackage."assoc")."1.0.2").revisions).default;
        "data-fix".revision = (((hackage."data-fix")."0.3.2").revisions).default;
        "alex".revision = (((hackage."alex")."3.2.7.1").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "OneTuple".revision = (((hackage."OneTuple")."0.3.1").revisions).default;
        "parsec".revision = (((hackage."parsec")."3.1.15.0").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.11.3.1").revisions).default;
        "gitrev".revision = (((hackage."gitrev")."1.3.1").revisions).default;
        "strict".revision = (((hackage."strict")."0.4.0.1").revisions).default;
        "strict".flags.assoc = true;
        "aeson".revision = (((hackage."aeson")."2.1.1.0").revisions).default;
        "aeson".flags.ordered-keymap = true;
        "aeson".flags.cffi = false;
        "tagged".revision = (((hackage."tagged")."0.8.6.1").revisions).default;
        "tagged".flags.deepseq = true;
        "tagged".flags.transformers = true;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.4").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "attoparsec".revision = (((hackage."attoparsec")."0.14.4").revisions).default;
        "attoparsec".flags.developer = false;
        "filepath".revision = (((hackage."filepath")."1.4.2.2").revisions).default;
        "th-lift".revision = (((hackage."th-lift")."0.8.2").revisions).default;
        "libyaml".revision = (((hackage."libyaml")."0.1.2").revisions).default;
        "libyaml".flags.system-libyaml = false;
        "libyaml".flags.no-unicode = false;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.2.0.1").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.2").revisions).default;
        "MemoTrie".revision = (((hackage."MemoTrie")."0.6.10").revisions).default;
        "MemoTrie".flags.examples = false;
        "resourcet".revision = (((hackage."resourcet")."1.3.0").revisions).default;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.8.0").revisions).default;
        "vector-stream".revision = (((hackage."vector-stream")."0.1.0.0").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."9.2.5").revisions).default;
        "indexed-traversable".revision = (((hackage."indexed-traversable")."0.1.2").revisions).default;
        "distributive".revision = (((hackage."distributive")."0.6.2.1").revisions).default;
        "distributive".flags.tagged = true;
        "distributive".flags.semigroups = true;
        "text-short".revision = (((hackage."text-short")."0.1.5").revisions).default;
        "text-short".flags.asserts = false;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.14").revisions).default;
        "bifunctors".flags.tagged = true;
        "bifunctors".flags.semigroups = true;
        "base".revision = (((hackage."base")."4.16.4.0").revisions).default;
        "time".revision = (((hackage."time")."1.11.1.1").revisions).default;
        "random".revision = (((hackage."random")."1.2.1.1").revisions).default;
        "process".revision = (((hackage."process")."1.6.16.0").revisions).default;
        "megaparsec".revision = (((hackage."megaparsec")."9.3.0").revisions).default;
        "megaparsec".flags.dev = false;
        "conduit".revision = (((hackage."conduit")."1.3.4.3").revisions).default;
        "vector-algorithms".revision = (((hackage."vector-algorithms")."0.9.0.1").revisions).default;
        "vector-algorithms".flags.internalchecks = false;
        "vector-algorithms".flags.llvm = false;
        "vector-algorithms".flags.properties = true;
        "vector-algorithms".flags.boundschecks = true;
        "vector-algorithms".flags.unsafechecks = false;
        "vector-algorithms".flags.bench = true;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.4.5.0").revisions).default;
        "semigroupoids".revision = (((hackage."semigroupoids")."5.3.7").revisions).default;
        "semigroupoids".flags.tagged = true;
        "semigroupoids".flags.containers = true;
        "semigroupoids".flags.distributive = true;
        "semigroupoids".flags.unordered-containers = true;
        "semigroupoids".flags.contravariant = true;
        "semigroupoids".flags.comonad = true;
        "these".revision = (((hackage."these")."1.1.1.1").revisions).default;
        "these".flags.assoc = true;
        "split".revision = (((hackage."split")."0.2.3.5").revisions).default;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.7").revisions).default;
        "ghc-bignum".revision = (((hackage."ghc-bignum")."1.2").revisions).default;
        "time-compat".revision = (((hackage."time-compat")."1.9.6.1").revisions).default;
        "time-compat".flags.old-locale = false;
        "primitive".revision = (((hackage."primitive")."0.7.4.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.2").revisions).default;
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.17.0.0").revisions).default;
        "optparse-applicative".flags.process = true;
        "rts".revision = (((hackage."rts")."1.0.2").revisions).default;
        "semialign".revision = (((hackage."semialign")."1.2.0.1").revisions).default;
        "semialign".flags.semigroupoids = true;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.18.0.0").revisions).default;
        "mono-traversable".revision = (((hackage."mono-traversable")."1.0.15.3").revisions).default;
        "witherable".revision = (((hackage."witherable")."0.4.2").revisions).default;
        "syb".revision = (((hackage."syb")."0.7.2.2").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.6.1").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "newtype-generics".revision = (((hackage."newtype-generics")."0.6.2").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.11.4").revisions).default;
        "ansi-terminal".flags.example = false;
        "ansi-terminal".flags.win32-2-13-1 = true;
        "hashable".revision = (((hackage."hashable")."1.4.1.0").revisions).default;
        "hashable".flags.containers = true;
        "hashable".flags.random-initial-seed = false;
        "hashable".flags.integer-gmp = true;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.7.2").revisions).default;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.three = false;
        "transformers-compat".flags.generic-deriving = true;
        "indexed-traversable-instances".revision = (((hackage."indexed-traversable-instances")."0.1.1.1").revisions).default;
        "scientific".revision = (((hackage."scientific")."0.3.7.0").revisions).default;
        "scientific".flags.bytestring-builder = false;
        "scientific".flags.integer-simple = false;
        "binary".revision = (((hackage."binary")."0.8.9.0").revisions).default;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.14.2").revisions).default;
        "QuickCheck".flags.old-random = false;
        "QuickCheck".flags.templatehaskell = true;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.5").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.5.1").revisions).default;
        "StateVar".revision = (((hackage."StateVar")."1.2.2").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.6").revisions).default;
        };
      compiler = {
        version = "9.2.5";
        nix-name = "ghc925";
        packages = {
          "pretty" = "1.1.3.6";
          "text" = "1.2.5.0";
          "array" = "0.5.4.0";
          "mtl" = "2.2.2";
          "parsec" = "3.1.15.0";
          "bytestring" = "0.11.3.1";
          "filepath" = "1.4.2.2";
          "stm" = "2.5.0.2";
          "ghc-prim" = "0.8.0";
          "ghc-boot-th" = "9.2.5";
          "base" = "4.16.4.0";
          "time" = "1.11.1.1";
          "process" = "1.6.16.0";
          "ghc-bignum" = "1.2";
          "directory" = "1.3.6.2";
          "exceptions" = "0.10.4";
          "rts" = "1.0.2";
          "transformers" = "0.5.6.2";
          "template-haskell" = "2.18.0.0";
          "deepseq" = "1.4.6.1";
          "unix" = "2.7.2.2";
          "binary" = "0.8.9.0";
          "containers" = "0.6.5.1";
          };
        };
      };
  extras = hackage:
    { packages = { fourmolu = ./.plan.nix/fourmolu.nix; }; };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "fourmolu" = {
            flags = {
              "dev" = lib.mkOverride 900 false;
              "fixity-th" = lib.mkOverride 900 true;
              };
            };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "Cabal-syntax".components.library.planned = lib.mkOverride 900 true;
          "ansi-terminal".components.library.planned = lib.mkOverride 900 true;
          "base-orphans".components.library.planned = lib.mkOverride 900 true;
          "megaparsec".components.library.planned = lib.mkOverride 900 true;
          "these".components.library.planned = lib.mkOverride 900 true;
          "resourcet".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "newtype-generics".components.library.planned = lib.mkOverride 900 true;
          "distributive".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "gitrev".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "mono-traversable".components.library.planned = lib.mkOverride 900 true;
          "strict".components.library.planned = lib.mkOverride 900 true;
          "comonad".components.library.planned = lib.mkOverride 900 true;
          "data-fix".components.library.planned = lib.mkOverride 900 true;
          "fourmolu".components.exes."fourmolu".planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "dlist".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "scientific".components.library.planned = lib.mkOverride 900 true;
          "splitmix".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "tagged".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "parser-combinators".components.library.planned = lib.mkOverride 900 true;
          "vector".components.library.planned = lib.mkOverride 900 true;
          "indexed-traversable-instances".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "Diff".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "StateVar".components.library.planned = lib.mkOverride 900 true;
          "case-insensitive".components.library.planned = lib.mkOverride 900 true;
          "vector-algorithms".components.library.planned = lib.mkOverride 900 true;
          "bitvec".components.library.planned = lib.mkOverride 900 true;
          "unliftio-core".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "yaml".components.library.planned = lib.mkOverride 900 true;
          "indexed-traversable".components.library.planned = lib.mkOverride 900 true;
          "ghc-lib-parser".components.library.planned = lib.mkOverride 900 true;
          "base-compat-batteries".components.library.planned = lib.mkOverride 900 true;
          "split".components.library.planned = lib.mkOverride 900 true;
          "contravariant".components.library.planned = lib.mkOverride 900 true;
          "syb".components.library.planned = lib.mkOverride 900 true;
          "text-short".components.library.planned = lib.mkOverride 900 true;
          "assoc".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "th-lift".components.library.planned = lib.mkOverride 900 true;
          "libyaml".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "witherable".components.library.planned = lib.mkOverride 900 true;
          "generically".components.library.planned = lib.mkOverride 900 true;
          "alex".components.exes."alex".planned = lib.mkOverride 900 true;
          "semialign".components.library.planned = lib.mkOverride 900 true;
          "QuickCheck".components.library.planned = lib.mkOverride 900 true;
          "ansi-wl-pprint".components.library.planned = lib.mkOverride 900 true;
          "uuid-types".components.library.planned = lib.mkOverride 900 true;
          "semigroupoids".components.library.planned = lib.mkOverride 900 true;
          "fourmolu".components.library.planned = lib.mkOverride 900 true;
          "attoparsec".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "th-abstraction".components.library.planned = lib.mkOverride 900 true;
          "attoparsec".components.sublibs."attoparsec-internal".planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "OneTuple".components.library.planned = lib.mkOverride 900 true;
          "th-lift-instances".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "primitive".components.library.planned = lib.mkOverride 900 true;
          "conduit".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "bifunctors".components.library.planned = lib.mkOverride 900 true;
          "unordered-containers".components.library.planned = lib.mkOverride 900 true;
          "random".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "vector-stream".components.library.planned = lib.mkOverride 900 true;
          "integer-logarithms".components.library.planned = lib.mkOverride 900 true;
          "transformers-compat".components.library.planned = lib.mkOverride 900 true;
          "colour".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "time-compat".components.library.planned = lib.mkOverride 900 true;
          "optparse-applicative".components.library.planned = lib.mkOverride 900 true;
          "aeson".components.library.planned = lib.mkOverride 900 true;
          "base-compat".components.library.planned = lib.mkOverride 900 true;
          "hashable".components.library.planned = lib.mkOverride 900 true;
          "MemoTrie".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }