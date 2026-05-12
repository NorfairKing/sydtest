{ stdenv, lib, compileMutationReport, assertMutationScore }:

# Run one or more test suites in mutation mode and fail if any mutation survives.
# Composition of compileMutationReport and assertMutationScore.

{ name # name for the derivation
, manifests # list of 'manifest' outputs from addManifest-wrapped packages
, testSuites # list of { executable, executableName }
}:

let
  report = compileMutationReport {
    inherit name manifests testSuites;
  };
in
assertMutationScore {
  name = "${name}-assert";
  inherit report;
}
