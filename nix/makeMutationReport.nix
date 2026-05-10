{ haskell
, callPackage
, addManifest'
, compileMutationReport
, assertMutationScore
}:

# High-level helper: instrument a package with the mutation plugin and run its
# test suite with --mutation against the resulting manifest.
#
# Returns a report derivation. Pass it to assertMutationScore to fail if any
# mutations survived, or inspect it directly.

{ name ? "mutation-report" # name for the report derivation
, package # name of the package under test (string)
, testPackage ? "${package}-gen" # name of the package whose test suite to run
, testExecutableName ? "${testPackage}-test" # name of the test executable
, exceptions ? [ ] # list of module names to exclude from instrumentation
, mustKillAll ? true # if true (default), fail if any mutations survive
}:

let
  addManifest = addManifest' { inherit exceptions; };

  # Override the package in the package set so instrumented version is used.
  addManifestOverride = _: super: {
    ${package} = addManifest super.${package};
  };

  newHaskellPackages = haskell.packages.ghc910.extend addManifestOverride;
  instrumentedPkg = newHaskellPackages.${package};
  testPkg = newHaskellPackages.${testPackage};

  report = compileMutationReport {
    inherit name testExecutableName;
    testExecutable = testPkg;
    inherit (instrumentedPkg) manifest;
  };
in
if mustKillAll
then assertMutationScore { name = "${name}-assert"; inherit report; }
else report
