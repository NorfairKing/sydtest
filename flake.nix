{
  description = "sydtest";
  nixConfig = {
    extra-substituters = "https://sydtest.cachix.org";
    extra-trusted-public-keys = "sydtest.cachix.org-1:fyby3c42t+0iTABcLd/R3POxzJhCQ/9gYM7Sh879+9w=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
    nixpkgs-22_05.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    nixpkgs-21_11.url = "github:NixOS/nixpkgs?ref=nixos-21.11";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-22_05
    , nixpkgs-21_11
    , pre-commit-hooks
    , autodocodec
    , validity
    , safe-coloured-text
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
          (import (validity + "/nix/overlay.nix"))
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.haskellPackages.sydtestRelease;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs:
            let pkgs' = pkgsFor nixpkgs;
            in pkgs'.haskellPackages.sydtestRelease;
          allNixpkgs = {
            inherit
              nixpkgs-22_05
              nixpkgs-21_11;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          release = self.packages.${system}.default;
          shell = self.devShells.${system}.default;
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ ".*/default.nix" ];
              cabal2nix.enable = true;
            };
          };
        };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "sydtest-shell";
        packages = p: builtins.attrValues p.sydtestPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; pkgs.haskellPackages.sydtest-webdriver.webdriverDeps ++ [
          cabal-install
          mongodb
          niv
          postgresql
          rabbitmq-server
          redis
          zlib
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = ''
          ${self.checks.${system}.pre-commit.shellHook}
          ${pkgs.haskellPackages.sydtest-webdriver.setupFontsConfigScript}
        '';
      };
    };
}
