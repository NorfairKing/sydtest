{ stdenv, lib, compileMutationReport, assertMutationScore }:

# Run the test suite in mutation mode and fail if any mutation survives.
# Composition of compileMutationReport and assertMutationScore.

{ name # name for the derivation
, manifest # the 'manifest' output of an addManifest-wrapped package
, testExecutable # derivation containing the test executable
, testExecutableName # name of the executable within testExecutable to invoke
}:

let
  report = compileMutationReport {
    inherit name manifest testExecutable testExecutableName;
  };
in
assertMutationScore {
  name = "${name}-assert";
  inherit report;
}
