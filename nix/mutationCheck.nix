{ haskellPackages, pkgs, addMutationRuntimeDependency }:

# Build one mutation check.
#
# Returns a derivation:
# - when assertAllKilled = true (the default): a check derivation that fails
#   the build if any mutations survive
# - when assertAllKilled = false: a report derivation containing report.txt
#   and report.json; succeeds as long as all test suites complete without
#   crashing
#
# Arguments:
# - name: derivation name prefix
# - packages: attr names in haskellPackages for packages that have both
#             a library to instrument and a test suite to run
# - libraries: attr names to instrument but not run test suites for
# - tests: attr names whose test suites to run but not instrument
# - needToBeLinkedAgainstMutationRuntime: attr names of packages whose
#       executables (test suites or benchmarks) link against an instrumented
#       library and therefore need sydtest-mutation-runtime added to their
#       link line. See ./nix/addMutationRuntimeDependency.nix.
# - config: attrset rendered to a YAML config file and consumed by the
#       mutation plugin (exceptions, disabled mutation types, skip-th-splices,
#       debug). Defaults to {}, meaning the plugin uses its built-in defaults.
#       Schema: Test.Syd.Mutation.Plugin.OptParse.MutationPluginConfig.
# - assertAllKilled: return a check derivation that fails if any mutations survive (default: true)
# - assertNoneUncovered: also fail the check derivation if any mutations are uncovered (default: true)
# - ghcMemLimit: RTS heap limit for GHC during instrumented compilation
# - coverageJobs: maximum number of coverage children to run concurrently.
#       Defaults to the build-sandbox's RTS capability count, which is too
#       aggressive for test suites that spawn expensive per-test resources
#       (e.g. tmp-postgres). Set this to cap the parallelism. 'null' leaves
#       the default in place.
# - coverageRetry: how many times to retry a failing coverage child before
#       giving up. Defaults to the harness default (3). Useful when test
#       suites flake on contended resources (e.g. tmp-postgres' port binding)
#       so the whole coverage phase doesn't get lost to a transient failure.
#       'null' leaves the harness default in place.
#
# The mutations are run inside the Cabal build's checkPhase of the first test
# package, so test resources (golden files, data files) are available at
# relative paths. All other test suite executables are referenced by Nix store
# paths baked in at evaluation time.
#
# The harness pulls testToolDepends from every test package automatically and
# puts them on PATH for its own checkPhase. This is necessary because the
# harness runs every test executable in turn from one checkPhase, so the
# per-package testToolDepends (which only fire during that package's own
# checkPhase, which the harness clears) are not otherwise visible to spawned
# test exes.

{ name
, packages ? [ ]
, libraries ? [ ]
, tests ? [ ]
, needToBeLinkedAgainstMutationRuntime ? [ ]
, config ? { }
, assertAllKilled ? true
, assertNoneUncovered ? true
, ghcMemLimit ? "16g"
, coverageJobs ? null
, coverageRetry ? null
  # Whether to enable fail-fast in the harness run. Defaults to false because
  # e2e checks want the full report (driven by assertMutationScore), and
  # fail-fast aborts the run on the first surviving/uncovered mutation which
  # discards report.json before the installPhase runs.
, failFast ? false
}:

let
  inherit (haskellPackages.sydtest) addManifest assertMutationScore;

  libraryPackages = packages ++ libraries;
  testPackages = packages ++ tests;

  # First, inject the sydtest-mutation-runtime build-dep into every package
  # the caller has listed. This ensures their executables link successfully
  # against any instrumented library they (transitively) depend on.
  addRuntimeOverride = _: super:
    builtins.listToAttrs (map
      (pkg: {
        name = pkg;
        value = addMutationRuntimeDependency super.${pkg};
      })
      needToBeLinkedAgainstMutationRuntime);

  # Then wrap the library packages with the mutation plugin so that
  # compilation emits ifMutation calls and writes a manifest.
  addManifestOverride = _: super:
    builtins.listToAttrs (map
      (pkg: {
        name = pkg;
        value = addManifest { inherit config ghcMemLimit; } super.${pkg};
      })
      libraryPackages);

  instrumentedHaskellPackages = haskellPackages.extend
    (pkgs.lib.composeExtensions addRuntimeOverride addManifestOverride);

  # Build a test package with doCheck=true so Cabal compiles the test
  # executable, then copy it to $out/test/ in postInstall. Cabal's 'copy'
  # command only installs library and executable components, not test suites,
  # so we copy them manually from dist/build/.
  # checkPhase is cleared so the tests are not run during this build; they are
  # run later in the mutation checkPhase of the first test package.
  # dontBenchmark: avoid building benchmark executables that would also end up
  # under dist/build/ and risk being picked up by the mutation harness's `find`.
  builtTestPkg = pkg:
    pkgs.haskell.lib.overrideCabal
      (pkgs.haskell.lib.dontBenchmark
        (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${pkg}))
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

  manifests = map (pkg: instrumentedHaskellPackages.${pkg}.manifest) libraryPackages;
  coverageFlags = pkgs.lib.concatMapStringsSep " "
    (m: "--mutation-coverage ${m}")
    manifests;
  coverageJobsFlag = pkgs.lib.optionalString (coverageJobs != null)
    "--mutation-coverage-jobs ${toString coverageJobs}";
  coverageRetryFlag = pkgs.lib.optionalString (coverageRetry != null)
    "--mutation-coverage-retry ${toString coverageRetry}";
  failFastFlag =
    if failFast
    then "--mutation-fail-fast"
    else "--no-mutation-fail-fast";
  mutationFlags = pkgs.lib.concatMapStringsSep " "
    (m: "--mutation ${m}")
    manifests;

  # The first test package is the one whose Cabal checkPhase we hijack to run
  # the mutation harness. Its executable is found at runtime via `find dist`
  # inside the build sandbox, so golden files and other relative paths work.
  firstTestPkg =
    if testPackages == [ ]
    then throw "sydtest.mutationCheck '${name}': no test suites configured. Provide at least one package under 'packages' or 'tests'."
    else builtins.head testPackages;
  # Extra test packages are built separately and referenced by their Nix store
  # paths; their executables are baked into the checkPhase script at eval time.
  extraTestPkgs = builtins.tail testPackages;

  # Path to the test executable directory for an extra (store-built) test package.
  storeTestDirOf = pkg: "${builtTestPkg pkg}/test";

  extraSuiteExeFlags = pkgs.lib.concatMapStringsSep " "
    (pkg: "--mutation-suite-exe ${pkg}=$(find ${storeTestDirOf pkg} -maxdepth 1 -type f | head -1)")
    extraTestPkgs;

  firstSuiteExeFlag = "--mutation-suite-exe ${firstTestPkg}=$(realpath \"$exe\")";

  allSuiteExeFlags = "${firstSuiteExeFlag} ${extraSuiteExeFlags}";

  extraCoverageScript = pkgs.lib.concatMapStringsSep "\n"
    (pkg: ''
      echo "mutation-nix: collecting coverage for suite ${pkg}"
      storeExe=$(find ${storeTestDirOf pkg} -maxdepth 1 -type f | head -1)
      "$storeExe" +RTS -M4g -RTS ${coverageFlags} ${coverageJobsFlag} ${coverageRetryFlag} \
        --mutation-suite-name ${pkg} \
        --mutation-augmented-manifest-dir augmented
    '')
    extraTestPkgs;

  extraInputs = map builtTestPkg extraTestPkgs;

  # Pull testToolDepends from every test package (first and extras) so they
  # are visible to the harness's checkPhase. Each package's own checkPhase is
  # cleared, so we cannot rely on per-package testToolDepends — they only fire
  # during that package's own check. We need them on PATH for the harness.
  # 'getCabalDeps.testToolDepends' is provided by haskellPackages.generic-builder
  # whenever doCheck = true; we apply doCheck before reading it.
  #
  # Caveat: this only works because we apply pkgs.haskell.lib.doCheck below
  # before reading getCabalDeps.testToolDepends. If a future caller pre-builds
  # the package without doCheck=true (or substitutes a derivation that does not
  # go through generic-builder), getCabalDeps will not be present and the
  # testToolDepends propagation will silently degrade to the empty list.
  collectedTestToolDepends = pkgs.lib.concatMap
    (pkg:
      let
        p = pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${pkg};
      in
        p.getCabalDeps.testToolDepends or [ ]
    )
    testPackages;

  drv =
    (pkgs.haskell.lib.overrideCabal
      # dontBenchmark: benchmarks are not relevant here and would waste build time.
      # doCheck: needed so Cabal compiles the test suite executable.
      (pkgs.haskell.lib.dontBenchmark
        (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${firstTestPkg}))
      (_old: {
        checkPhase = ''
          # Find the test executable inside the Cabal build tree. Filtering .so
          # files excludes Haskell shared libraries that Cabal also places under
          # dist/build/ and would otherwise be mistaken for executables.
          exe=$(find dist -type f -executable | grep -v '\.so' | head -1)
          mkdir -p augmented
          echo "mutation-nix: collecting coverage for suite ${firstTestPkg}"
          # +RTS -M4g -RTS: cap the test process heap to 4 GB to avoid OOM in
          # the Nix sandbox, where memory is shared with the builder.
          "$exe" +RTS -M4g -RTS ${coverageFlags} ${coverageJobsFlag} ${coverageRetryFlag} \
            --mutation-suite-name ${firstTestPkg} \
            --mutation-augmented-manifest-dir augmented
          ${extraCoverageScript}
          echo "mutation-nix: running mutations"
          mkdir -p $report
          "$exe" ${mutationFlags} \
            --mutation-augmented-manifest-dir augmented \
            ${allSuiteExeFlags} \
            --mutation-child-mem-limit 4g \
            ${failFastFlag} \
            --mutation-report-dir "$report" | tee $report/report.txt
        '';
        # Suppress the default postCheck phase so it does not re-run the test
        # suite after our mutation checkPhase has already finished.
        postCheck = "";
      })).overrideAttrs (old: {
      # extraInputs are the extra test package derivations; adding them to
      # buildInputs ensures they are present in the sandbox PATH during checkPhase.
      buildInputs = (old.buildInputs or [ ]) ++ extraInputs;
      # Test executables spawn tools (postgresql, git, nix, ...) at runtime
      # via testToolDepends. The harness runs every test exe from a single
      # checkPhase, so we put the union of those deps from every test package
      # on PATH here — per-package testToolDepends only fire during that
      # package's own checkPhase, which the harness clears.
      nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ collectedTestToolDepends;
      # Add a 'report' output so callers can access report.txt and report.json
      # without having to build the full 'out' derivation.
      outputs = (old.outputs or [ "out" ]) ++ [ "report" ];
    });

  report = drv.report;
  check = assertMutationScore { inherit name assertNoneUncovered; report = drv.report; };
in
if assertAllKilled
then check
else report
