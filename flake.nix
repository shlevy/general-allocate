{
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    pre-commit-hooks-nix.url = "github:shlevy/pre-commit-hooks.nix/hs-boot";
  };

  outputs = {
    self,
    flake-parts,
    haskell-nix,
    nixpkgs,
    pre-commit-hooks-nix,
    ...
  }:
    flake-parts.lib.mkFlake {inherit self;} ({
      config,
      lib,
      withSystem,
      ...
    }: let
      projects = import ./nix/materialized-project.nix {
        inherit (config) systems;
        inherit lib;
        pkgsBySystem = system: withSystem system ({pkgs, ...}: pkgs);
        projectArgs = {
          src = ./.;
          compiler-nix-name = "ghc925";
          shell.tools = {
            cabal = {version = "3.8.1.0";};
            fourmolu = {version = "0.10.1.0";};
          };
        };
      };

      inherit (lib) mkMerge;
    in {
      imports = [
        pre-commit-hooks-nix.flakeModule
      ];

      systems = ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"];

      perSystem = {
        system,
        config,
        ...
      }: let
        inherit (projects.${system}) project update-materialized;

        flake = project.flake {};
      in {
        inherit (flake) checks packages;

        apps =
          flake.apps
          // {
            inherit update-materialized;
          };

        devShells =
          flake.devShells
          // {
            default = lib.overrideDerivation flake.devShells.default (orig: {
              shellHook =
                (orig.shellHook or "")
                + ''
                  ${config.pre-commit.installationScript}
                '';
            });
          };

        pre-commit.settings.hooks = {
          alejandra = {
            enable = true;
            excludes = ["materialized/"];
          };
          fourmolu.enable = true;
          hlint.enable = true;
        };

        pre-commit.settings.tools.fourmolu = lib.mkForce (project.tools project.args.shell.tools).fourmolu;

        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [haskell-nix.overlay];
          inherit (haskell-nix) config;
        };
      };
    });

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
