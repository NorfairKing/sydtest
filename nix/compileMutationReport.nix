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
    # Copy manifest dirs to writable locations for the coverage phase.
    ${lib.concatImapStrings (i: m: ''
      manifest_dir_${toString i}=$(mktemp -d)
      cp -r ${m}/. "$manifest_dir_${toString i}/"
      chmod -R u+w "$manifest_dir_${toString i}"
    '') manifests}
    echo "mutation-nix: collecting per-test coverage"
    (
      ${lib.optionalString (testResourcesDir != null) "cd ${testResourcesDir}"}
      ${lib.getExe' testExecutable testExecutableName} \
        ${lib.concatImapStringsSep " " (i: _m: "--mutation-coverage \"$manifest_dir_${toString i}\"") manifests}
    )
    echo "mutation-nix: running mutations"
    (
      ${lib.optionalString (testResourcesDir != null) "cd ${testResourcesDir}"}
      ${lib.getExe' testExecutable testExecutableName} \
        ${lib.concatImapStringsSep " " (i: _m: "--mutation \"$manifest_dir_${toString i}\"") manifests}
    ) | tee report.txt
  '';

  installPhase = ''
    mkdir -p $out
    cp report.txt $out/report.txt
  '';
}
