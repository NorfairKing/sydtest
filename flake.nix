{
  description = "sydtest";
  nixConfig = {
    extra-substituters = "https://sydtest.cachix.org";
    extra-trusted-public-keys = "sydtest.cachix.org-1:fyby3c42t+0iTABcLd/R3POxzJhCQ/9gYM7Sh879+9w=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.11";
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
    really-safe-money.url = "github:NorfairKing/really-safe-money";
    really-safe-money.flake = false;
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
    , really-safe-money
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
        (pkgs.callPackage (really-safe-money + "/nix/overrides.nix") { })
        self.overrides.${system}
      ];
      horizonPkgs = horizon-advance.legacyPackages.${system}.extend allOverrides;
      haskellPackages = pkgs.haskellPackages.extend allOverrides;
    in
    {
      overrides.${system} = pkgs.callPackage ./nix/overrides.nix { };
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = haskellPackages.sydtestRelease;
      checks.${system} = {
        forwardCompatibility = horizonPkgs.sydtestRelease;
        release = haskellPackages.sydtestRelease;
        mutation-release = haskellPackages.sydtestMutationRelease;
        mutation-report-really-safe-money =
          let
            addManifest = haskellPackages.mutationNixPackages.addManifest { };
            # Instrument really-safe-money and thread the instrumented version
            # through so really-safe-money-gen picks it up as a dependency.
            instrumentedHaskellPackages = haskellPackages.extend (_: super: {
              really-safe-money = addManifest super.really-safe-money;
            });
            instrumentedLib = instrumentedHaskellPackages.really-safe-money;
            # Build really-safe-money-gen and install the test executable to $out/bin.
            # doCheck = true so cabal includes test deps; checkPhase is skipped (the
            # mutation runner invokes the binary directly via compileMutationReport).
            testPkg = pkgs.haskell.lib.overrideCabal
              (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.really-safe-money-gen)
              (old: {
                checkPhase = ''
                  find . -name "really-safe-money-test" -type f -exec install -Dm755 {} $out/bin/really-safe-money-test \;
                '';
              });
            compileMutationReport = haskellPackages.mutationNixPackages.compileMutationReport;
            assertMutationScore = haskellPackages.mutationNixPackages.assertMutationScore;
          in
          assertMutationScore {
            name = "mutation-report-really-safe-money-assert";
            report = compileMutationReport {
              name = "mutation-report-really-safe-money";
              testExecutable = testPkg;
              testExecutableName = "really-safe-money-test";
              manifest = instrumentedLib.manifest;
            };
          };
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
