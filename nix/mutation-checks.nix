{ haskellPackages, pkgs }:

# End-to-end mutation testing checks for packages that sydtest itself depends on.
#
# Each check instruments the library packages with the mutation plugin, builds
# the corresponding test executables against the instrumented libraries, runs
# the suites in mutation mode, and writes report.txt and report.json to the
# 'report' output.
#
# The checks do NOT fail on surviving mutations; they only fail if a test suite
# crashes or the derivation build fails. Inspect the 'report' output to see
# which mutations survived. This makes CI green even when mutation scores are
# not perfect, while still surfacing the data.
#
# Each entry uses `.report` to select the 'report' multi-output from the
# derivation returned by mutationCheck, so `nix build .#checks.x86_64-linux.mutation-X`
# builds only the report output and not the full test package.

let
  mutationCheck = pkgs.callPackage ./mutationCheck.nix { inherit haskellPackages; };
in
{
  # TODO: autodocodec mutation check takes too long to run, investigate
  # mutation-autodocodec = (mutationCheck {
  #   name = "autodocodec";
  #   packages = [
  #     "autodocodec"
  #     "autodocodec-api-usage"
  #   ];
  #   libraries = [
  #     "autodocodec-exact"
  #     "autodocodec-nix"
  #     "autodocodec-openapi3"
  #     "autodocodec-schema"
  #     "autodocodec-servant-multipart"
  #     "autodocodec-swagger2"
  #     "autodocodec-yaml"
  #   ];
  #   debug = true;
  # }).report;

  mutation-opt-env-conf = (mutationCheck {
    name = "opt-env-conf";
    packages = [ "opt-env-conf-test" ];
    libraries = [ "opt-env-conf" ];
    debug = true;
  }).report;

  mutation-really-safe-money = (mutationCheck {
    name = "really-safe-money";
    libraries = [
      "really-safe-money"
      "really-safe-money-autodocodec"
    ];
    tests = [
      "really-safe-money-gen"
    ];
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
  }).check;
}
