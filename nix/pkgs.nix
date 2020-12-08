let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv { };
  yamlparse-applicative-overlay =
    import (
      pkgs.fetchFromGitHub (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  sydtestPkgs =
    pkgsv {
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
