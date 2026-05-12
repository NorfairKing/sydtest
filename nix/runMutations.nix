{ stdenv, lib, compileMutationReport, assertMutationScore }:

# Run one or more test suite packages in mutation mode and fail if any mutation survives.
# Composition of compileMutationReport and assertMutationScore.

{ name # name for the derivation
, manifests # list of 'manifest' outputs from addManifest-wrapped packages
, testPackages # list of instrumented Haskell package derivations with test suite executables
}:

let
  report = compileMutationReport {
    inherit name manifests testPackages;
  };
in
assertMutationScore {
  name = "${name}-assert";
  inherit report;
}
