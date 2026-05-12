{ haskellPackages, pkgs }:

# End-to-end mutation testing checks.
#
# Each check instruments one or more libraries with the mutation plugin, builds
# the corresponding test executable against the instrumented libraries, runs the
# suite in mutation mode, and writes report.txt and report.json to the 'report'
# output. The checks succeed as long as the test suite runs without crashing;
# inspect the report outputs to see which mutations survived.

let
  mutationCheck = pkgs.callPackage ./mutationCheck.nix { inherit haskellPackages; };
in
{
  mutation-really-safe-money = (mutationCheck {
    name = "really-safe-money";
    libraryPackages = [ "really-safe-money" ];
    testPackage = "really-safe-money-gen";
    testExecutableName = "really-safe-money-test";
    debug = true;
  }).report;

  mutation-safe-coloured-text = (mutationCheck {
    name = "safe-coloured-text";
    libraryPackages = [ "safe-coloured-text" "safe-coloured-text-parsing" ];
    testPackage = "safe-coloured-text-gen";
    testExecutableName = "safe-coloured-text-test";
    debug = true;
  }).report;
}
