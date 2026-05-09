{ stdenv, lib, compileMutationReport, assertMutationScore }:

# Run the mutation runner against a manifest and fail if any mutation survives.
# This is the composition of compileMutationReport and assertMutationScore.

{ name # name for the derivation
, manifest # the 'manifest' output of an addManifest-wrapped package; contains mutation.manifest
, runner # derivation containing the runner executable
, runnerExecutable # name of the executable within runner to invoke
}:

let
  report = compileMutationReport {
    inherit name manifest runner runnerExecutable;
  };
in
assertMutationScore {
  name = "${name}-assert";
  inherit report;
}
