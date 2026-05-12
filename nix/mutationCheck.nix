{ haskellPackages, pkgs }:

# Build one mutation check.
#
# - name: derivation name prefix
# - libraryPackages: list of attr names in haskellPackages for the libraries to instrument
# - testSuites: list of { package, executableName } attrsets for the test suites to run
#   (backward compat: testPackage + testExecutableName are still accepted as a
#   single-suite shorthand and are internally converted to testSuites)
# - exceptions: list of module names to skip during instrumentation (applies to all libraries)
#
# The mutations are run inside the Cabal build's checkPhase, so test resources
# (golden files, data files) are available at the expected relative paths.
# All test suite executables beyond the first are referenced as Nix store paths.

{ name
, libraryPackages
, testSuites ? null
, testPackage ? null
, testExecutableName ? null
, exceptions ? [ ]
, debug ? false
, ghcMemLimit ? "16g"
}:

let
  # Normalise to a list of { package, executableName }.
  resolvedTestSuites =
    if testSuites != null
    then testSuites
    else if testPackage != null && testExecutableName != null
    then [{ package = testPackage; executableName = testExecutableName; }]
    else throw "mutationCheck: supply either testSuites or both testPackage and testExecutableName";

  inherit (haskellPackages.sydtest) addManifest;
  instrumentedHaskellPackages = haskellPackages.extend (_: super:
    builtins.listToAttrs (map
      (pkg: {
        name = pkg;
        value = addManifest { inherit exceptions debug ghcMemLimit; } super.${pkg};
      })
      libraryPackages));
  manifests = map (pkg: instrumentedHaskellPackages.${pkg}.manifest) libraryPackages;
  coverageFlags = pkgs.lib.concatMapStringsSep " "
    (m: "--mutation-coverage ${m}")
    manifests;
  mutationFlags = pkgs.lib.concatMapStringsSep " "
    (m: "--mutation ${m}")
    manifests;

  # The first test suite drives the parent mutation process (runs in-tree via find dist).
  firstSuite = builtins.head resolvedTestSuites;

  # Extra suites beyond the first — these use Nix store paths.
  extraSuites = builtins.tail resolvedTestSuites;

  # Build the --mutation-suite-exe flags: first suite uses in-tree exe path
  # (set at runtime); extra suites use Nix store paths.
  extraSuiteExeFlags = pkgs.lib.concatMapStringsSep " "
    (s: "--mutation-suite-exe ${s.executableName}=${pkgs.lib.getExe' instrumentedHaskellPackages.${s.package} s.executableName}")
    extraSuites;

  # For the first suite, the exe flag is set at runtime inside checkPhase.
  # For the parent's --mutation-suite-exe for the first suite, we also set it.
  firstSuiteExeFlag = "--mutation-suite-exe ${firstSuite.executableName}=$(realpath \"$exe\")";

  allSuiteExeFlags = "${firstSuiteExeFlag} ${extraSuiteExeFlags}";

  # Coverage phase for extra suites (Nix store paths).
  extraCoverageScript = pkgs.lib.concatMapStringsSep "\n"
    (s:
      let storeExe = pkgs.lib.getExe' instrumentedHaskellPackages.${s.package} s.executableName;
      in ''
        echo "mutation-nix: collecting coverage for suite ${s.executableName}"
        "${storeExe}" +RTS -M4g -RTS ${coverageFlags} \
          --mutation-suite-name ${s.executableName} \
          --mutation-augmented-manifest-dir augmented
      '')
    extraSuites;

  extraInputs = map (s: instrumentedHaskellPackages.${s.package}) extraSuites;
in
(pkgs.haskell.lib.overrideCabal
  (pkgs.haskell.lib.dontBenchmark
    (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${firstSuite.package}))
  (_old: {
    buildInputs = extraInputs;
    checkPhase = ''
      exe=$(find dist -name "${firstSuite.executableName}" -type f | head -1)
      mkdir -p augmented
      echo "mutation-nix: collecting coverage for suite ${firstSuite.executableName}"
      "$exe" +RTS -M4g -RTS ${coverageFlags} \
        --mutation-suite-name ${firstSuite.executableName} \
        --mutation-augmented-manifest-dir augmented
      ${extraCoverageScript}
      echo "mutation-nix: running mutations"
      mkdir -p $report
      "$exe" ${mutationFlags} \
        --mutation-augmented-manifest-dir augmented \
        ${allSuiteExeFlags} \
        --mutation-child-mem-limit 4g \
        --mutation-report-dir "$report" | tee $report/report.txt
    '';
    postCheck = "";
  })).overrideAttrs (old: {
  outputs = (old.outputs or [ "out" ]) ++ [ "report" ];
})
