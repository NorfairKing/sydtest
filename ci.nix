{ pkgs ? import ./nix/pkgs.nix { } }:
let
  sources = import ./nix/sources.nix;

  versions = {
    "lts-16_11" = "89db531aea80df58584c9a9e3504ffd9617e6b48";
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
  pre-commit-check = (import ./nix/pre-commit.nix).check;
} // builtins.mapAttrs mkReleaseForVersion versions
