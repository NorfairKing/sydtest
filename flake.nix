{
  description = "sydtest";
  nixConfig = {
    extra-substituters = "https://sydtest.cachix.org";
    extra-trusted-public-keys = "sydtest.cachix.org-1:fyby3c42t+0iTABcLd/R3POxzJhCQ/9gYM7Sh879+9w=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-26.05";
    horizon-advance.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-advance";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , horizon-advance
    , pre-commit-hooks
    , autodocodec
    , validity
    , safe-coloured-text
    , fast-myers-diff
    , opt-env-conf
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
      allOverrides = pkgs.lib.composeManyExtensions [
        (pkgs.callPackage (fast-myers-diff + "/nix/overrides.nix") { })
        (pkgs.callPackage (autodocodec + "/nix/overrides.nix") { })
        (pkgs.callPackage (safe-coloured-text + "/nix/overrides.nix") { })
        (pkgs.callPackage (validity + "/nix/overrides.nix") { })
        (pkgs.callPackage (opt-env-conf + "/nix/overrides.nix") { })
        self.overrides.${system}
      ];
      horizonPkgs = horizon-advance.legacyPackages.${system}.extend allOverrides;
      haskellPackages = pkgs.haskellPackages.extend allOverrides;
    in
    {
      overrides.${system} = pkgs.callPackage ./nix/overrides.nix { };
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = haskellPackages.sydtestRelease;
        # Diff-scoped mutation runner for the in-repo example check.
        # 'nix run .#mutation-sydtest-mutation-example-diff' mutation-tests
        # only the mutations implied by the current diff.  See
        # nix/mutationCheck.nix's '.diff' passthru.
        mutation-sydtest-mutation-example-diff =
          self.checks.${system}.mutation-sydtest-mutation-example.diff;
        # Redundant-test analyser for the in-repo example check.
        # 'nix run .#mutation-sydtest-mutation-example-redundancy' reports
        # tests that are redundant with respect to mutation testing
        # ('--basis coverage' by default; pass '--basis kill' for the
        # accurate kill matrix).  See nix/mutationCheck.nix's '.redundancy'
        # passthru.
        mutation-sydtest-mutation-example-redundancy =
          self.checks.${system}.mutation-sydtest-mutation-example.redundancy;
      };
      checks.${system} = {
        forwardCompatibility = horizonPkgs.sydtestReleaseWithoutMutation;
        release = haskellPackages.sydtestRelease;
        mutation-manifest-example = pkgs.callPackage ./nix/mutationManifestCheck.nix { inherit haskellPackages; };
      } // (import ./nix/mutation-checks.nix {
        inherit haskellPackages;
      }) // {
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
      devShells.${system}.default = haskellPackages.shellFor {
        name = "sydtest-shell";
        packages = p: builtins.attrValues p.sydtestPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; haskellPackages.sydtest-webdriver.webdriverDeps ++ [
          cabal-install
          postgresql
          redis
          sqitchPg
          # mongodb
          zlib
        ]) ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = ''
          ${self.checks.${system}.pre-commit.shellHook}
          ${haskellPackages.sydtest-webdriver.setupFontsConfigScript}
        '';
      };
      nix-ci.cachix = {
        name = "sydtest";
        public-key = "sydtest.cachix.org-1:fyby3c42t+0iTABcLd/R3POxzJhCQ/9gYM7Sh879+9w=";
      };
    };
}
