{ pkgsf ? import (import ./nixpkgs.nix { }) }:
let
  pkgs = pkgsf { };
  yamlparse-applicative-overlay =
    import (
      pkgs.fetchFromGitHub (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  sydtestPkgs =
    pkgsf {
      overlays =
        [
          yamlparse-applicative-overlay
          (import ./gitignore-src.nix)
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
    };
in
sydtestPkgs
