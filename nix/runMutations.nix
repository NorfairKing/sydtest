{ compileMutationReport, assertMutationScore }:

# Run one or more test suite packages in mutation mode and fail if any mutation
# survives. Thin composition of compileMutationReport and assertMutationScore.
# Prefer this over calling them separately when you do not need to inspect the
# report on a failed build.

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
