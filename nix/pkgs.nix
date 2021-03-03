{ pkgsf ? import (import ./nixpkgs.nix { }) }:
let
  pkgs = pkgsf { };
  yamlparse-applicative-overlay =
    import (
      builtins.fetchGit (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  safe-coloured-text-overlay =
    import (
      builtins.fetchGit (import ./safe-coloured-text-version.nix) + "/nix/overlay.nix"
    );
  sydtestPkgs =
    pkgsf {
      overlays =
        [
          yamlparse-applicative-overlay
          safe-coloured-text-overlay
          (import ./gitignore-src.nix)
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
    };
in
sydtestPkgs
