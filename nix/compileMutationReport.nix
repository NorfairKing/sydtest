{ stdenv, lib }:

# Run the test suite in mutation mode against a manifest and produce a report.
# Always succeeds — use assertMutationScore to fail on surviving mutations.

{ name # name for the derivation
, manifest # the 'manifest' output of an addManifest-wrapped package
, testExecutable # derivation containing the test executable
, testExecutableName # name of the executable within testExecutable to invoke
}:

stdenv.mkDerivation {
  name = "${name}-mutation-report";

  dontUnpack = true;

  buildInputs = [ testExecutable ];

  buildPhase = ''
    echo "mutation-nix: running mutations from ${manifest}"
    ${lib.getExe' testExecutable testExecutableName} \
      --mutation "${manifest}" \
      | tee report.txt
  '';

  installPhase = ''
    mkdir -p $out
    cp report.txt $out/report.txt
  '';
}
