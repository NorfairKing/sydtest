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
    # Copy manifest dirs to writable locations so the coverage phase can write
    # .coverage.json files alongside the existing manifest files.
    writable_manifests=()
    ${lib.concatMapStrings (m: ''
      manifest_copy=$(mktemp -d)
      cp -r "${m}/." "$manifest_copy/"
      chmod -R u+w "$manifest_copy"
      writable_manifests+=("$manifest_copy")
    '') manifests}
    coverage_flags=$(printf -- '--mutation-coverage "%s" ' "''${writable_manifests[@]}")
    mutation_flags=$(printf -- '--mutation "%s" ' "''${writable_manifests[@]}")
    echo "mutation-nix: collecting per-test coverage"
    (
      ${lib.optionalString (testResourcesDir != null) "cd ${testResourcesDir}"}
      ${lib.getExe' testExecutable testExecutableName} $coverage_flags
    )
    echo "mutation-nix: running mutations"
    (
      ${lib.optionalString (testResourcesDir != null) "cd ${testResourcesDir}"}
      ${lib.getExe' testExecutable testExecutableName} $mutation_flags
    ) | tee report.txt
  '';

  installPhase = ''
    mkdir -p $out
    cp report.txt $out/report.txt
  '';
}
