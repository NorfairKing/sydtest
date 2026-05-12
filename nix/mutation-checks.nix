{ haskellPackages, pkgs }:

# End-to-end mutation testing checks.
#
# Each check instruments library packages with the mutation plugin, builds
# the corresponding test executables against the instrumented libraries, runs
# the suites in mutation mode, and writes report.txt and report.json to the
# 'report' output. The checks succeed as long as the test suites run without
# crashing; inspect the report outputs to see which mutations survived.

let
  mutationCheck = pkgs.callPackage ./mutationCheck.nix { inherit haskellPackages; };
in
{
  mutation-really-safe-money = (mutationCheck {
    name = "really-safe-money";
    libraries = [ "really-safe-money" ];
    tests = [ "really-safe-money-gen" ];
    debug = true;
  }).report;

  mutation-safe-coloured-text = (mutationCheck {
    name = "safe-coloured-text";
    libraries = [
      "safe-coloured-text"
      "safe-coloured-text-layout"
      "safe-coloured-text-parsing"
      "safe-coloured-text-terminfo"
    ];
    tests = [
      "safe-coloured-text-gen"
      "safe-coloured-text-layout-gen"
      "safe-coloured-text-parsing-gen"
    ];
    debug = true;
  }).report;

  mutation-sydtest-mutation-example = (mutationCheck {
    name = "sydtest-mutation-example";
    packages = [
      "sydtest-mutation-example"
      "sydtest-mutation-example-gen"
    ];
  }).report;
}
