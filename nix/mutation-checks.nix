{ haskellPackages, pkgs }:

# End-to-end mutation testing checks.
#
# Each check instruments a library with the mutation plugin, builds the
# corresponding test executable against the instrumented library, runs the
# suite in mutation mode, and fails if any mutation survived.

let
  inherit (haskellPackages.mutationNixPackages) addManifest compileMutationReport assertMutationScore;

  # Build one mutation check.
  #
  # - libraryPackage: attr name in haskellPackages for the library to instrument
  # - testPackage: attr name in haskellPackages for the package containing the test binary
  # - testExecutableName: name of the executable inside testPackage
  # - exceptions: list of module names to skip during instrumentation
  mutationCheck =
    { name
    , libraryPackage
    , testPackage
    , testExecutableName
    , exceptions ? [ ]
    }:
    let
      instrumentedHaskellPackages = haskellPackages.extend (_: super: {
        ${libraryPackage} = addManifest { inherit exceptions; } super.${libraryPackage};
      });
      instrumentedLib = instrumentedHaskellPackages.${libraryPackage};
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
        inherit testExecutableName;
        manifest = instrumentedLib.manifest;
      };
    };

in
{
  mutation-really-safe-money = mutationCheck {
    name = "really-safe-money";
    libraryPackage = "really-safe-money";
    testPackage = "really-safe-money-gen";
    testExecutableName = "really-safe-money-test";
  };
}
