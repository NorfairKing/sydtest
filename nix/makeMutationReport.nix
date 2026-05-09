{ haskellPackages
, addManifest'
, compileMutationReport
, assertMutationScore
}:
let x = haskellPackages; # Trick we have to use for naming conflicts
in

# High-level helper: compile a package with the mutation plugin and run its
  # runner executable against the resulting manifest.
  #
  # Returns a report derivation. Pass it to assertMutationScore to fail if any
  # mutations survived, or inspect it directly to see the results.
  #
  # NOTE: Currently only a single package is supported. The natural extension
  # (mirroring dekking's 'packages', 'coverables', and 'coverage' lists) is to
  # accept a list of package names whose manifests are merged before the runner
  # is invoked. See addManifest.nix and compileMutationReport.nix for where that
  # merging should happen.

{ name ? "mutation-report" # name for the report derivation
, haskellPackages ? x # the haskell package set to draw packages from
, package # name of the package under test (string); must expose a runner executable
, runnerExecutable ? "${package}-runner" # name of the runner executable within the package
, exceptions ? [ ] # list of module names to exclude from instrumentation
, mustKillAll ? true # if true (default), fail if any mutations survive
}:

let
  addManifest = addManifest' { inherit exceptions; };

  # Override the package in the package set so that reverse dependencies
  # also pick up the instrumented version (same pattern as dekking).
  addManifestOverride = _: super: {
    ${package} = addManifest super.${package};
  };

  newHaskellPackages = haskellPackages.extend addManifestOverride;
  instrumentedPkg = newHaskellPackages.${package};

  report = compileMutationReport {
    inherit name runnerExecutable;
    runner = instrumentedPkg;
    inherit (instrumentedPkg) manifest;
  };
in
if mustKillAll
then
  assertMutationScore
  {
    name = "${name}-assert";
    inherit report;
  }
else report
