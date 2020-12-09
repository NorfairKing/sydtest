{ pkgs ? import ./nix/pkgs.nix }:
let
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
}
