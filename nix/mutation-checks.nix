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
# Entries that should not fail on surviving mutations pass
# `assertAllKilled = false`, so `mutationCheck` returns the report derivation
# directly. Entries that should fail on surviving mutations use the default
# `assertAllKilled = true`, so `mutationCheck` returns the check derivation.

let
  # Single source of truth for plugin-instrumentation tunables. Rendered to
  # YAML and passed to every mutation check via '--config=PATH'.
  # Schema: Test.Syd.Mutation.Plugin.OptParse.MutationPluginConfig
  config = {
    debug = true;
  };

  mutationCheck = args: pkgs.callPackage ./mutationCheck.nix
    {
      inherit haskellPackages;
      inherit (haskellPackages.sydtest) addMutationRuntimeDependency;
    }
    ({ inherit config; } // args);
in
{
  # TODO: autodocodec mutation check takes too long to run, investigate
  # mutation-autodocodec = mutationCheck {
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
  #   assertAllKilled = false;
  # };

  mutation-opt-env-conf = mutationCheck {
    name = "opt-env-conf";
    packages = [ "opt-env-conf-test" ];
    libraries = [ "opt-env-conf" ];
    assertAllKilled = false;
  };

  mutation-safe-coloured-text = mutationCheck {
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
    assertAllKilled = false;
  };

  mutation-sydtest-mutation-example = mutationCheck {
    name = "sydtest-mutation-example";
    libraries = [ "sydtest-mutation-example" ];
    tests = [ "sydtest-mutation-example-gen" ];
  };
}
