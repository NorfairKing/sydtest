{ haskellPackages, pkgs, safe-coloured-text }:

# End-to-end mutation testing checks.
#
# Each check instruments one or more libraries with the mutation plugin, builds
# the corresponding test executable against the instrumented libraries, runs the
# suite in mutation mode, and fails if any mutation survived.

let
  inherit (haskellPackages.mutationNixPackages) addManifest compileMutationReport assertMutationScore;

  # Build one mutation check.
  #
  # - libraryPackages: list of attr names in haskellPackages for the libraries to instrument
  # - testPackage: attr name in haskellPackages for the package containing the test binary
  # - testExecutableName: name of the executable inside testPackage
  # - exceptions: list of module names to skip during instrumentation (applies to all libraries)
  mutationCheck =
    { name
    , libraryPackages
    , testPackage
    , testExecutableName
    , exceptions ? [ ]
    , testResourcesDir ? null
    }:
    let
      instrumentedHaskellPackages = haskellPackages.extend (_: super:
        builtins.listToAttrs (map
          (pkg: {
            name = pkg;
            value = addManifest { inherit exceptions; } super.${pkg};
          })
          libraryPackages));
      manifests = map (pkg: instrumentedHaskellPackages.${pkg}.manifest) libraryPackages;
      testPkg = pkgs.haskell.lib.overrideCabal
        (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${testPackage})
        (_old: {
          checkPhase = ''
            find . -name "${testExecutableName}" -type f -exec install -Dm755 {} $out/bin/${testExecutableName} \;
          '';
        });
    in
    assertMutationScore {
      name = "mutation-${name}-assert";
      report = compileMutationReport {
        name = "mutation-${name}";
        testExecutable = testPkg;
        inherit testExecutableName manifests testResourcesDir;
      };
    };

in
{
  mutation-really-safe-money = mutationCheck {
    name = "really-safe-money";
    libraryPackages = [ "really-safe-money" ];
    testPackage = "really-safe-money-gen";
    testExecutableName = "really-safe-money-test";
  };

  mutation-safe-coloured-text = mutationCheck {
    name = "safe-coloured-text";
    libraryPackages = [ "safe-coloured-text" "safe-coloured-text-parsing" ];
    testPackage = "safe-coloured-text-gen";
    testExecutableName = "safe-coloured-text-test";
    testResourcesDir = "${safe-coloured-text}/safe-coloured-text-gen";
  };
}
