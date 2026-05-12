{ haskellPackages, pkgs }:

# Build one mutation check.
#
# - name: derivation name prefix
# - libraryPackages: list of attr names in haskellPackages for the libraries to instrument
# - testSuites: list of { package, executableName } attrsets for the test suites to run
# - exceptions: list of module names to skip during instrumentation (applies to all libraries)
#
# The mutations are run inside the Cabal build's checkPhase, so test resources
# (golden files, data files) are available at the expected relative paths.
# All test suite executables beyond the first are referenced as Nix store paths.

{ name
, libraryPackages
, testSuites
, exceptions ? [ ]
, debug ? false
, ghcMemLimit ? "16g"
}:

let
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

  # The first test suite drives the parent mutation process (in-tree via find dist).
  firstSuite = builtins.head testSuites;
  extraSuites = builtins.tail testSuites;

  # Extra suites use Nix store paths; the first suite's path is resolved at runtime.
  extraSuiteExeFlags = pkgs.lib.concatMapStringsSep " "
    (s: "--mutation-suite-exe ${s.executableName}=${pkgs.lib.getExe' instrumentedHaskellPackages.${s.package} s.executableName}")
    extraSuites;

  firstSuiteExeFlag = "--mutation-suite-exe ${firstSuite.executableName}=$(realpath \"$exe\")";

  allSuiteExeFlags = "${firstSuiteExeFlag} ${extraSuiteExeFlags}";

  # Coverage for extra suites uses Nix store paths.
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
