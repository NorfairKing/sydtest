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
, exceptions ? [ ] # list of module names to exclude from instrumentation
, disabledMutations ? [ ] # list of mutation type names to disable globally
, mustKillAll ? true # if true (default), fail if any mutations survive
}:

let
  addManifest = addManifest' { inherit exceptions disabledMutations; };

  addManifestOverride = _: super: {
    ${package} = addManifest super.${package};
  };

  newHaskellPackages = haskell.packages.ghc910.extend addManifestOverride;
  instrumentedPkg = newHaskellPackages.${package};

  builtTestPkg =
    haskell.lib.overrideCabal
      (haskell.lib.doCheck newHaskellPackages.${testPackage})
      (_: {
        checkPhase = "";
        postInstall = ''
          for exe in dist/build/*/*; do
            [ -f "$exe" ] && [ -x "$exe" ] || continue
            mkdir -p $out/test
            cp "$exe" $out/test/
          done
        '';
      });

  report = compileMutationReport {
    inherit name;
    testPackages = [ builtTestPkg ];
    manifests = [ instrumentedPkg.manifest ];
  };
in
if mustKillAll
then assertMutationScore { name = "${name}-assert"; inherit report; }
else report
