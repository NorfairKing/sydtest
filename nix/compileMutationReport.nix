{ stdenv, lib }:

# Run the test suite in mutation mode against one or more manifest directories
# and produce a report.
# Always succeeds — use assertMutationScore to fail on surviving mutations.

{ name # name for the derivation
, manifests # list of 'manifest' outputs from addManifest-wrapped packages
, testExecutable # derivation containing the test executable
, testExecutableName # name of the executable within testExecutable to invoke
, testResourcesDir ? null # optional directory to cd into before running the test (for golden test resources)
}:

stdenv.mkDerivation {
  name = "${name}-mutation-report";

  dontUnpack = true;

  buildInputs = [ testExecutable ];

  buildPhase = ''
    mkdir -p augmented
    echo "mutation-nix: collecting per-test coverage"
    (
      ${lib.optionalString (testResourcesDir != null) "cd ${testResourcesDir}"}
      ${lib.getExe' testExecutable testExecutableName} +RTS -M4g -RTS \
        ${lib.concatMapStringsSep " " (m: "--mutation-coverage ${m}") manifests} \
        --mutation-augmented-manifest-dir augmented
    )
    echo "mutation-nix: running mutations"
    (
      ${lib.optionalString (testResourcesDir != null) "cd ${testResourcesDir}"}
      ${lib.getExe' testExecutable testExecutableName} +RTS -M4g -RTS \
        ${lib.concatMapStringsSep " " (m: "--mutation ${m}") manifests} \
        --mutation-augmented-manifest-dir augmented \
        --mutation-report-dir augmented
    ) | tee report.txt
  '';

  installPhase = ''
    mkdir -p $out
    cp report.txt $out/report.txt
    cp augmented/report.json $out/report.json
  '';
}
