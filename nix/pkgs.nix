{ pkgsf ? import ./nixpkgs.nix }:
let
  sources = import ./sources.nix;
  pkgs = pkgsf { };
  pre-commit-hooks = import sources.pre-commit-hooks;
  yamlparse-applicative-overlay = import (sources.yamlparse-applicative + "/nix/overlay.nix");
  safe-coloured-text-overlay = import (sources.safe-coloured-text + "/nix/overlay.nix");
  sydtestPkgs =
    pkgsf {
      overlays =
        [
          yamlparse-applicative-overlay
          safe-coloured-text-overlay
          (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
    };
in
sydtestPkgs
