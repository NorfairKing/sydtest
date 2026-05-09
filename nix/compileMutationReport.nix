{ stdenv, lib }:

# Run the mutation runner against a manifest and produce a report derivation.
# Always succeeds — use assertMutationScore to fail on surviving mutations.
#
# NOTE: This currently takes a single manifest. Once multi-package manifests
# are supported (see addManifest.nix), this should accept a list of manifest
# paths and concatenate them, so that one runner invocation covers mutations
# from the whole codebase.

{ name # name for the derivation
, manifest # the 'manifest' output of an addManifest-wrapped package; contains mutation.manifest
, runner # derivation containing the runner executable
, runnerExecutable # name of the executable within runner to invoke
}:

stdenv.mkDerivation {
  name = "${name}-mutation-report";

  # No source needed — we only need the manifest and the runner.
  dontUnpack = true;

  buildInputs = [ runner ];

  buildPhase = ''
    echo "mutation-nix: running mutations from ${manifest}/mutation.manifest"
    ${lib.getExe' runner runnerExecutable} "${manifest}/mutation.manifest" \
      | tee report.txt
  '';

  installPhase = ''
    mkdir -p $out
    cp report.txt $out/report.txt
  '';
}
