{ haskellPackages }:

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
  # Plugin-instrumentation tunables for every in-repo mutation check live
  # in ./mutation.yaml. Passed to the plugin via '--config=PATH'.
  # Schema: Test.Syd.Mutation.Plugin.OptParse.MutationPluginConfig
  configFile = ./mutation.yaml;

  mutationCheck = args:
    haskellPackages.sydtest.mutationCheck ({ inherit configFile; } // args);
in
{
  # NOTE: libraries that sydtest (and therefore the mutation plugin/driver)
  # itself depends on cannot be instrumented here — the plugin would load
  # the mutated version of its own dep on the second build pass and either
  # crash or hang.  This currently rules out:
  #
  #   * opt-env-conf      (the plugin uses it to parse its own settings)
  #   * safe-coloured-text and its layout/parsing/terminfo siblings
  #                       (sydtest uses these for colour output, which the
  #                       mutation driver invokes during report rendering)
  #
  # If you want to mutate any of these again, the plugin/driver needs to be
  # made independent of that library first.

  mutation-sydtest-mutation-example = mutationCheck {
    name = "sydtest-mutation-example";
    libraries = [ "sydtest-mutation-example" ];
    tests = [ "sydtest-mutation-example-gen" ];
    # Opt in (the default is off) so the redundant-test analysis is exercised
    # end-to-end in CI: this run also produces redundancy.txt/redundancy.json.
    redundancy = true;
  };
}
