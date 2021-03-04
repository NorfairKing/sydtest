{ pkgs ? import ./nix/pkgs.nix { } }:
let
  versions = [
    "lts-14_23"
    "lts-15_03"
    "lts-16_11"
    "lts-16_20"
  ];

  mkReleaseForVersion = version:
    let
      nixpkgsVersion = import (./ci + "/${version}.nix");
      pkgsf = import (import ./nix/nixpkgs.nix { inherit nixpkgsVersion; });
      p = import ./nix/pkgs.nix { inherit pkgsf; };
    in
    p.sydtestRelease;

  nix-pre-commit-hooks =
    import (
      builtins.fetchTarball "https://github.com/hercules-ci/nix-pre-commit-hooks/archive/1b11ce0f8c65dd3d8e9520e23c100b76d09a858b.tar.gz"
    );
in
{
  release = pkgs.sydtestRelease;
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      nixpkgs-fmt.enable = true;
      hlint.enable = true;
      ormolu.enable = true;
    };
  };
} // pkgs.lib.genAttrs versions mkReleaseForVersion
