{ haskellPackages, pkgs }:

# End-to-end mutation testing checks.
#
# Each check instruments one or more libraries with the mutation plugin, builds
# the corresponding test executables against the instrumented libraries, runs the
# suites in mutation mode, and writes report.txt and report.json to the 'report'
# output. The checks succeed as long as the test suites run without crashing;
# inspect the report outputs to see which mutations survived.

let
  mutationCheck = pkgs.callPackage ./mutationCheck.nix { inherit haskellPackages; };
in
{
  mutation-really-safe-money = (mutationCheck {
    name = "really-safe-money";
    libraryPackages = [ "really-safe-money" ];
    testSuites = [{ package = "really-safe-money-gen"; executableName = "really-safe-money-test"; }];
    debug = true;
  }).report;

  mutation-safe-coloured-text = (mutationCheck {
    name = "safe-coloured-text";
    libraryPackages = [
      "safe-coloured-text"
      "safe-coloured-text-layout"
      "safe-coloured-text-parsing"
    ];
    testSuites = [
      { package = "safe-coloured-text-gen"; executableName = "safe-coloured-text-test"; }
      { package = "safe-coloured-text-layout-gen"; executableName = "safe-coloured-text-layout-test"; }
    ];
    debug = true;
  }).report;

  mutation-sydtest-mutation-example = (mutationCheck {
    name = "sydtest-mutation-example";
    libraryPackages = [ "sydtest-mutation-example" ];
    testSuites = [
      { package = "sydtest-mutation-example"; executableName = "sydtest-mutation-example-test"; }
      { package = "sydtest-mutation-example-gen"; executableName = "sydtest-mutation-example-gen-test"; }
    ];
  }).report;
}
