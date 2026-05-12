{ stdenv, lib }:

# Run one or more test suites in mutation mode against one or more manifest
# directories and produce a report.
# Always succeeds — use assertMutationScore to fail on surviving mutations.

{ name # name for the derivation
, manifests # list of 'manifest' outputs from addManifest-wrapped packages
, testSuites ? null # list of { executable, executableName } for multi-suite runs
, testExecutable ? null # (backward compat) single test suite derivation
, testExecutableName ? null # (backward compat) executable name within testExecutable
, testResourcesDir ? null # optional directory to cd into before running the test (for golden test resources)
}:

let
  # Normalise to a list of { executable, executableName }.
  resolvedTestSuites =
    if testSuites != null
    then testSuites
    else if testExecutable != null && testExecutableName != null
    then [{ executable = testExecutable; executableName = testExecutableName; }]
    else throw "compileMutationReport: supply either testSuites or both testExecutable and testExecutableName";

  firstSuite = builtins.head resolvedTestSuites;

  coverageFlags = lib.concatMapStringsSep " " (m: "--mutation-coverage ${m}") manifests;
  mutationFlags = lib.concatMapStringsSep " " (m: "--mutation ${m}") manifests;

  suiteExeFlags = lib.concatMapStringsSep " "
    (s: "--mutation-suite-exe ${s.executableName}=${lib.getExe' s.executable s.executableName}")
    resolvedTestSuites;

  # Coverage phase: each suite runs sequentially, merging into augmented/.
  coveragePhaseScript = lib.concatMapStringsSep "\n"
    (s: ''
      echo "mutation-nix: collecting coverage for suite ${s.executableName}"
      (
        ${lib.optionalString (testResourcesDir != null) "cd ${testResourcesDir}"}
        ${lib.getExe' s.executable s.executableName} +RTS -M4g -RTS \
          ${coverageFlags} \
          --mutation-suite-name ${s.executableName} \
          --mutation-augmented-manifest-dir augmented
      )
    '')
    resolvedTestSuites;

in
stdenv.mkDerivation {
  name = "${name}-mutation-report";

  dontUnpack = true;

  buildInputs = map (s: s.executable) resolvedTestSuites;

  buildPhase = ''
    mkdir -p augmented
    ${coveragePhaseScript}
    echo "mutation-nix: running mutations"
    (
      ${lib.optionalString (testResourcesDir != null) "cd ${testResourcesDir}"}
      ${lib.getExe' firstSuite.executable firstSuite.executableName} +RTS -M4g -RTS \
        ${mutationFlags} \
        --mutation-augmented-manifest-dir augmented \
        ${suiteExeFlags} \
        --mutation-report-dir augmented
    ) | tee report.txt
  '';

  installPhase = ''
    mkdir -p $out
    cp report.txt $out/report.txt
    cp augmented/report.json $out/report.json
  '';
}
