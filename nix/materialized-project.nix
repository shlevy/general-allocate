{
  projectRoot ? projectArgs.src,
  materializedRelative ? "nix/materialized",
  systems,
  projectArgs ? null,
  projectArgsBySystem ? _: projectArgs,
  lib ? pkgs.lib,
  pkgsBySystem ? system:
    if system == pkgs.pkgsBuildBuild.system
    then pkgs
    else
      import pkgs.path {
        inherit system;
        inherit (pkgs) overlays;
      },
  pkgs ? null,
} @ args:
assert (args ? projectArgs) || (args ? projectArgsBySystem);
assert (args ? pkgs) || (args ? pkgsBySystem);
assert (args ? lib) || (args ? pkgs);
assert (args ? projectRoot) || (args ? projectArgs.src); let
  inherit (lib) mapAttrs optionalAttrs optionalString concatStringsSep;

  ifExists = p:
    if builtins.pathExists p
    then p
    else null;

  materializedRoot = /${projectRoot}/${materializedRelative};

  bySystem = {
    evalSystem ? system,
    system,
  }: let
    pkgs = pkgsBySystem system;

    projectArgs = projectArgsBySystem system;

    materializedFor = tool:
      if tool == null
      then ifExists /${materializedRoot}/${system}/project
      else ifExists /${materializedRoot}/${system}/tools/${tool};

    tools = mapAttrs (name: value:
      {
        inherit (project) index-state evalSystem;
        materialized = materializedFor "tool-${name}";
      }
      // value)
    projectArgs.shell.tools;

    hasTools = projectArgs ? shell.tools;

    project = pkgs.haskell-nix.cabalProject' ({
        materialized = materializedFor null;
        inherit evalSystem;
      }
      // projectArgs
      // optionalAttrs hasTools {
        shell =
          projectArgs.shell
          // {
            inherit tools;
          };
      });

    tools-built = project.tools tools;

    evalPkgs = project.pkg-set.config.evalPackages;

    inherit (evalPkgs) writeShellScript;
  in {
    inherit project;

    update-materialized-system = writeShellScript "update-materialized-${system}" ''
      set -eEuo pipefail
      mkdir -p "${materializedRelative}/${system}"
      cd "${materializedRelative}/${system}"
      echo "Updating project materialization" >&2
      ${project.plan-nix.passthru.generateMaterialized} project
      ${optionalString hasTools (concatStringsSep "\n" (map (tool: ''
        echo "Updating ${tool} tool materialization" >&2
        mkdir -p tools
        ${tools-built.${tool}.project.plan-nix.passthru.generateMaterialized} tools/${tool}
      '') (builtins.attrNames tools-built)))}
    '';
  };
in
  builtins.listToAttrs (map (system: {
      name = system;
      value = {
        project = (bySystem {inherit system;}).project;

        update-materialized = let
          pkgs = pkgsBySystem system;

          inherit (pkgs) git writeShellScript;
        in {
          type = "app";

          program =
            (writeShellScript "update-materialized" ''
              set -eEuo pipefail
              cd "$(${git}/bin/git rev-parse --show-toplevel)"
              ${concatStringsSep "\n" (map (otherSystem: ''
                  echo "Updating materialization for ${otherSystem}" >&2
                  ${(bySystem {
                      evalSystem = system;
                      system = otherSystem;
                    })
                    .update-materialized-system}
                '')
                systems)}
            '')
            .outPath;
        };
      };
    })
    systems)
