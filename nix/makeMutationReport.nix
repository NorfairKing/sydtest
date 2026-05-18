{ haskell
, haskellPackages
  # addManifest' is the partially-applied addManifest function exported from
  # nix/addManifest.nix; the prime suffix signals that it still needs an
  # options attrset before it can be applied to a package.
, addManifest'
, compileMutationReport
, assertMutationScore
}:

# High-level helper: instrument a single library package with the mutation
# plugin and run its companion test suite with --mutation against the manifest.
#
# Intended for external callers (e.g. other flakes that depend on sydtest).
# For in-tree checks, prefer mutationCheck.nix which integrates more tightly
# with the Cabal build and supports multiple packages in one run.
#
# Returns a report derivation (or an assertMutationScore derivation when
# mustKillAll = true). Pass the report to assertMutationScore separately if you
# want to decouple building from checking.

{ name ? "mutation-report" # name for the report derivation
, package # attr name of the library to instrument (string, must exist in haskellPackages)
, testPackage ? "${package}-gen" # attr name of the package whose test suite to run; defaults to <package>-gen
, config ? { } # attrset rendered to a YAML config file for the plugin (schema: Test.Syd.Mutation.Plugin.OptParse.MutationPluginConfig)
, mustKillAll ? true # if true (default), wrap result in assertMutationScore and fail if any mutations survive
}:

let
  addManifest = addManifest' { inherit config; };

  addManifestOverride = _: super: {
    ${package} = addManifest super.${package};
  };

  newHaskellPackages = haskellPackages.extend addManifestOverride;
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
