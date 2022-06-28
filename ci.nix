{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
, pkgs ? import ./nix/pkgs.nix { inherit sources nixpkgs system; }
}:
let
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };

  versions = {
    "nixos-21_11" = sources.nixpkgs-21_11;
    "nixos-22_05" = sources.nixpkgs-22_05;
  };

  mkReleaseForVersion = version: nixpkgs:
    let
      p = import ./nix/pkgs.nix {
        inherit sources nixpkgs system;
      };
    in
    p.sydtestRelease.overrideAttrs (old: { name = "sydtest-release-${version}"; });
in
{
  release = pkgs.sydtestRelease;
  hoogle = pkgs.buildEnv {
    name = "sydtest-hoogle";
    paths = [ (pkgs.haskellPackages.ghcWithHoogle (ps: pkgs.lib.attrValues pkgs.sydtestPackages)) ];
  };
  shell = pkgs.symlinkJoin {
    name = "sydtest-shell";
    paths = (import ./shell.nix { inherit sources pkgs pre-commit; }).buildInputs;
  };
  pre-commit-check = pre-commit.check;
} // builtins.mapAttrs mkReleaseForVersion versions
