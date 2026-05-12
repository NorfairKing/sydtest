{ stdenv, lib }:

# Run one or more test suites in mutation mode against one or more manifest
# directories and produce a report.
# Always succeeds — use assertMutationScore to fail on surviving mutations.

{ name # name for the derivation
, manifests # list of 'manifest' outputs from addManifest-wrapped packages
, testSuites # list of { executable, executableName }
, testResourcesDir ? null # optional directory to cd into before running (for golden test resources)
}:

let
  firstSuite = builtins.head testSuites;

  coverageFlags = lib.concatMapStringsSep " " (m: "--mutation-coverage ${m}") manifests;
  mutationFlags = lib.concatMapStringsSep " " (m: "--mutation ${m}") manifests;

  suiteExeFlags = lib.concatMapStringsSep " "
    (s: "--mutation-suite-exe ${s.executableName}=${lib.getExe' s.executable s.executableName}")
    testSuites;

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
    testSuites;

in
stdenv.mkDerivation {
  name = "${name}-mutation-report";

  dontUnpack = true;

  buildInputs = map (s: s.executable) testSuites;

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
