{ haskellPackages, pkgs }:

# End-to-end mutation testing checks.
#
# Each check instruments one or more libraries with the mutation plugin, builds
# the corresponding test executable against the instrumented libraries, runs the
# suite in mutation mode, and fails if any mutation survived.

let
  inherit (haskellPackages.mutationNixPackages) addManifest assertMutationScore;

  # Build one mutation check.
  #
  # - libraryPackages: list of attr names in haskellPackages for the libraries to instrument
  # - testPackage: attr name in haskellPackages for the package containing the test binary
  # - testExecutableName: name of the executable inside testPackage
  # - exceptions: list of module names to skip during instrumentation (applies to all libraries)
  #
  # The mutations are run inside the Cabal build's checkPhase, so test resources
  # (golden files, data files) are available at the expected relative paths.
  mutationCheck =
    { name
    , libraryPackages
    , testPackage
    , testExecutableName
    , exceptions ? [ ]
    }:
    let
      instrumentedHaskellPackages = haskellPackages.extend (_: super:
        builtins.listToAttrs (map
          (pkg: {
            name = pkg;
            value = addManifest { inherit exceptions; } super.${pkg};
          })
          libraryPackages));
      manifests = map (pkg: instrumentedHaskellPackages.${pkg}.manifest) libraryPackages;
      # Manifest dirs are in the Nix store (read-only).  Copy them to writable
      # temp dirs so the coverage phase can write .coverage.json files alongside
      # the existing manifest files, and the mutation phase can read them back.
      copyManifests = pkgs.lib.concatMapStrings
        (m: ''
          manifest_copy=$(mktemp -d)
          cp -r "${m}/." "$manifest_copy/"
          chmod -R u+w "$manifest_copy"
          coverage_flags+=("--mutation-coverage" "$manifest_copy")
          mutation_flags+=("--mutation" "$manifest_copy")
        '')
        manifests;
      # Run the test suite in mutation mode inside the Cabal build's checkPhase,
      # where the working directory contains test_resources/ and other data files.
      # The report is written to the 'report' output so assertMutationScore can read it.
      report = ((pkgs.haskell.lib.overrideCabal
        (pkgs.haskell.lib.dontBenchmark
          (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${testPackage}))
        (_old: {
          checkPhase = ''
            exe=$(find dist -name "${testExecutableName}" -type f | head -1)
            # Copy manifest dirs to writable locations for coverage output.
            coverage_flags=()
            mutation_flags=()
            ${copyManifests}
            echo "mutation-nix: collecting per-test coverage"
            "$exe" "''${coverage_flags[@]}"
            echo "mutation-nix: running mutations"
            mkdir -p $report
            "$exe" "''${mutation_flags[@]}" | tee $report/report.txt
          '';
          postCheck = "";
        })).overrideAttrs (old: {
        outputs = (old.outputs or [ "out" ]) ++ [ "report" ];
      }));
    in
    assertMutationScore {
      name = "mutation-${name}-assert";
      report = report.report;
    };

in
{
  mutation-really-safe-money = mutationCheck {
    name = "really-safe-money";
    libraryPackages = [ "really-safe-money" ];
    testPackage = "really-safe-money-gen";
    testExecutableName = "really-safe-money-test";
  };

  mutation-safe-coloured-text = mutationCheck {
    name = "safe-coloured-text";
    libraryPackages = [ "safe-coloured-text" "safe-coloured-text-parsing" ];
    testPackage = "safe-coloured-text-gen";
    testExecutableName = "safe-coloured-text-test";
  };
}
