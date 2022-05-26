{ pkgs ? import ./nix/pkgs.nix { } }:
let
  sources = import ./nix/sources.nix;
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };

  versions = {
    "nixos-21_11" = "5a2e2471e8163da8e6f2c1dfd50ef9063199c08b";
  };

  mkReleaseForVersion = version: rev:
    let
      pkgsf = import (builtins.fetchGit {
        url = "https://github.com/NixOS/nixpkgs";
        inherit rev;
      });
      p = import ./nix/pkgs.nix { inherit pkgsf; };
    in
    p.sydtestRelease.overrideAttrs (old: { name = "sydtest-release-${version}"; });
in
{
  release = pkgs.sydtestRelease;
  hoogle = pkgs.sydtestHoogle;
  shell = pkgs.symlinkJoin {
    name = "sydtest-shell";
    paths = (import ./shell.nix { inherit sources pkgs pre-commit; }).buildInputs;
  };
  pre-commit-check = pre-commit.check;
} // builtins.mapAttrs mkReleaseForVersion versions
