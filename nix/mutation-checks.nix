{ haskellPackages, pkgs }:

# End-to-end mutation testing checks.
#
# Each check instruments one or more libraries with the mutation plugin, builds
# the corresponding test executable against the instrumented libraries, runs the
# suite in mutation mode, and writes report.txt and report.json to the 'report'
# output. The checks succeed as long as the test suite runs without crashing;
# inspect the report outputs to see which mutations survived.

let
  inherit (haskellPackages.mutationNixPackages) addManifest;

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
      # The manifest dirs live in the Nix store (read-only). We copy each one to
      # a writable temp dir so the coverage phase can write .coverage.json files
      # next to the manifest files, and the mutation phase can read them back.
      # Each manifest dir gets its own copy so the paths stay stable.
      setupWritableManifests = pkgs.lib.concatImapStrings
        (i: m: ''
          manifest_dir_${toString i}=$(mktemp -d)
          cp -r ${m}/. "$manifest_dir_${toString i}/"
          chmod -R u+w "$manifest_dir_${toString i}"
        '')
        manifests;
      coverageFlags = pkgs.lib.concatImapStringsSep " "
        (i: _m: "--mutation-coverage \"$manifest_dir_${toString i}\"")
        manifests;
      mutationFlags = pkgs.lib.concatImapStringsSep " "
        (i: _m: "--mutation \"$manifest_dir_${toString i}\"")
        manifests;
      # Run the test suite in mutation mode inside the Cabal build's checkPhase,
      # where the working directory contains test_resources/ and other data files.
      # report.txt (human-readable) and report.json (machine-readable) are written
      # to the 'report' output.
    in
    (pkgs.haskell.lib.overrideCabal
      (pkgs.haskell.lib.dontBenchmark
        (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${testPackage}))
      (_old: {
        checkPhase = ''
          exe=$(find dist -name "${testExecutableName}" -type f | head -1)
          ${setupWritableManifests}
          echo "mutation-nix: collecting per-test coverage"
          "$exe" +RTS -M4g -RTS ${coverageFlags}
          echo "mutation-nix: running mutations"
          mkdir -p $report
          "$exe" ${mutationFlags} --mutation-child-mem-limit 4g --mutation-report-dir "$report" | tee $report/report.txt
        '';
        postCheck = "";
      })).overrideAttrs (old: {
      outputs = (old.outputs or [ "out" ]) ++ [ "report" ];
    });

in
{
  mutation-really-safe-money = (mutationCheck {
    name = "really-safe-money";
    libraryPackages = [ "really-safe-money" ];
    testPackage = "really-safe-money-gen";
    testExecutableName = "really-safe-money-test";
  }).report;

  mutation-safe-coloured-text = (mutationCheck {
    name = "safe-coloured-text";
    libraryPackages = [ "safe-coloured-text" "safe-coloured-text-parsing" ];
    testPackage = "safe-coloured-text-gen";
    testExecutableName = "safe-coloured-text-test";
  }).report;
}
