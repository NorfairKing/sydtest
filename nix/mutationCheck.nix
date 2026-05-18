{ haskellPackages, pkgs, addMutationRuntimeDependency, cabalComponents }:

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
# - testProcessMemLimit: RTS heap limit for the test process during the
#       coverage phase and for each mutation child during the mutation phase
#       (default: "4g")
# - coverageJobs: maximum number of coverage children to run concurrently.
#       Defaults to 4, a conservative cap that keeps mutation checks from
#       OOM-ing on hosts where 'getNumCapabilities' is high but test suites
#       spawn expensive per-test resources (e.g. tmp-postgres). Pass 'null'
#       to fall back to the harness default of 'getNumCapabilities'.
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
  # Source directory roots searched (in order) by 'cabalComponents.forPackage'
  # to locate each listed package's <pname>.cabal at Nix evaluation time.
  # The cabal files are read to discover executable and test-suite stanza
  # names; those names drive what addManifest builds and how the harness
  # invokes test executables.  Common roots: the flake itself and any
  # input flakes that provide source-tree subdirectories.
, sources ? [ ]
, config ? { }
, assertAllKilled ? true
, assertNoneUncovered ? true
, ghcMemLimit ? "16g"
  # RTS heap cap for the test process during the coverage phase and for each
  # mutation child during the mutation phase.  Passed as '+RTS -M<limit> -RTS'
  # to the test exe and as '--mutation-child-mem-limit' to the harness.
, testProcessMemLimit ? "4g"
, coverageJobs ? 4
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

  # Cabal-discovered components for every package this check touches.
  # Looked up once at eval time from the configured 'sources' roots so
  # nothing here depends on the (tarballed) haskellPackages source
  # derivations.
  components =
    cabalComponents.forPackages sources
      (pkgs.lib.unique (libraryPackages ++ testPackages));
  executablesOf = pkgName: components.${pkgName}.executables;
  testSuiteNamesOf = pkgName: components.${pkgName}.testSuites;

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
  # compilation emits ifMutation calls and writes a manifest.  Each
  # package's declared 'executable' components are also built (silenced)
  # and installed; see nix/addManifest.nix.  Test-suites are NOT built
  # here: the outer doCheck=true wrapper below rebuilds the test
  # package with --enable-tests, which is where test exes are produced.
  addManifestOverride = _: super:
    builtins.listToAttrs (map
      (pkg: {
        name = pkg;
        value = addManifest
          {
            inherit config ghcMemLimit;
            executables = executablesOf pkg;
          }
          super.${pkg};
      })
      libraryPackages);

  instrumentedHaskellPackages = haskellPackages.extend
    (pkgs.lib.composeExtensions addRuntimeOverride addManifestOverride);

  # Build a test package with doCheck=true so Cabal compiles every
  # test-suite, then copy each test executable to
  # @$out/test/<test-suite-name>@ in postInstall.  Cabal's 'copy' command
  # only installs library and executable components, not test suites, so
  # we copy them manually from their predictable paths under dist/build/.
  # checkPhase is cleared so the tests are not run during this build;
  # they are run later in the mutation checkPhase of the first
  # test-suite.  dontBenchmark: avoid building benchmark executables
  # that would also end up under dist/build/.
  builtTestPkg = pkgName:
    let suites = testSuiteNamesOf pkgName;
    in
    pkgs.haskell.lib.overrideCabal
      (pkgs.haskell.lib.dontBenchmark
        (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${pkgName}))
      (_: {
        checkPhase = "";
        postInstall = ''
          mkdir -p $out/test
        '' + pkgs.lib.concatMapStringsSep "\n"
          (suite: ''
            src="dist/build/${suite}/${suite}"
            if [ -f "$src" ] && [ -x "$src" ]; then
              cp "$src" "$out/test/${suite}"
            else
              echo "mutation-nix: expected test executable at $src but it is missing" >&2
              exit 1
            fi
          '')
          suites;
      });

  # Expand the caller's @testPackages@ list to a flat list of
  # @{ pkg, suite }@ records, one per declared test-suite. The harness
  # treats each test-suite as an independent "suite" with its own
  # --mutation-suite-name / --mutation-suite-exe flags.
  allTestSuites =
    pkgs.lib.concatMap
      (pkgName: map (suite: { pkg = pkgName; inherit suite; }) (testSuiteNamesOf pkgName))
      testPackages;

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

  # The first test-suite is the one whose Cabal checkPhase we hijack to
  # run the mutation harness.  Its executable lives at a predictable path
  # inside the in-place Cabal build tree, so golden files and other
  # relative paths resolve as Cabal expects.
  firstSuite =
    if allTestSuites == [ ]
    then throw "sydtest.mutationCheck '${name}': no test-suites declared by any of packages = ${toString testPackages} or tests = ${toString tests}. Provide at least one package that declares a test-suite."
    else builtins.head allTestSuites;
  # Every other declared test-suite is built separately and referenced by
  # its Nix store path; the executable path is baked into the checkPhase
  # script at eval time.
  extraSuites = builtins.tail allTestSuites;

  # On-disk path of a test exe inside a built test package.
  storeTestExePath = s: "${builtTestPkg s.pkg}/test/${s.suite}";

  extraSuiteExeFlags = pkgs.lib.concatMapStringsSep " "
    (s: "--mutation-suite-exe ${s.suite}=${storeTestExePath s}")
    extraSuites;

  firstSuiteExeFlag = "--mutation-suite-exe ${firstSuite.suite}=$(realpath \"$exe\")";

  allSuiteExeFlags = "${firstSuiteExeFlag} ${extraSuiteExeFlags}";

  extraCoverageScript = pkgs.lib.concatMapStringsSep "\n"
    (s: ''
      echo "mutation-nix: collecting coverage for suite ${s.suite}"
      "${storeTestExePath s}" +RTS -M${testProcessMemLimit} -RTS ${coverageFlags} ${coverageJobsFlag} ${coverageRetryFlag} ${failFastFlag} \
        --mutation-suite-name ${s.suite} \
        --mutation-augmented-manifest-dir augmented
    '')
    extraSuites;

  # Distinct packages needed on buildInputs.  Multiple extra suites from
  # the same package share a single derivation.
  extraInputs =
    let pkgsNeeded = pkgs.lib.unique (map (s: s.pkg) extraSuites);
    in map builtTestPkg pkgsNeeded;

  # Pull testToolDepends from every test package (first and extras) so they
  # are visible to the harness's checkPhase. Each package's own checkPhase is
  # cleared, so we cannot rely on per-package testToolDepends — they only fire
  # during that package's own check. We need them on PATH for the harness.
  # 'getCabalDeps.testToolDepends' is provided by haskellPackages.generic-builder
  # whenever doCheck = true; we apply doCheck before reading it.
  #
  # We assert that getCabalDeps is present rather than defaulting silently:
  # if a future caller substitutes a derivation that does not go through
  # generic-builder, the eval fails loudly here rather than producing a
  # build whose test exes silently lack their runtime PATH dependencies.
  collectedTestToolDepends = pkgs.lib.concatMap
    (pkg:
      let
        p = pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${pkg};
      in
      assert pkgs.lib.assertMsg (p ? getCabalDeps)
        "sydtest.mutationCheck '${name}': package '${pkg}' has no getCabalDeps attribute; it must go through haskellPackages.generic-builder.";
      p.getCabalDeps.testToolDepends
    )
    testPackages;

  drv =
    (pkgs.haskell.lib.overrideCabal
      # dontBenchmark: benchmarks are not relevant here and would waste build time.
      # doCheck: needed so Cabal compiles the test suite executable.
      (pkgs.haskell.lib.dontBenchmark
        (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${firstSuite.pkg}))
      (_old: {
        checkPhase = ''
          # The first test-suite's executable lives at a predictable path
          # inside Cabal's in-place build tree.  Fail loudly if it is not
          # there rather than silently picking some other binary.
          exe="dist/build/${firstSuite.suite}/${firstSuite.suite}"
          if [ ! -x "$exe" ]; then
            echo "mutation-nix: expected test executable at $exe but it is not executable" >&2
            exit 1
          fi
          mkdir -p augmented
          echo "mutation-nix: collecting coverage for suite ${firstSuite.suite}"
          # +RTS -M${testProcessMemLimit} -RTS: cap the test process heap to
          # avoid OOM in the Nix sandbox, where memory is shared with the
          # builder.
          # ${failFastFlag} is forwarded so the coverage phase mirrors the
          # caller's fail-fast intent. With fail-fast on, a baseline test
          # failure exits code 2 and aborts the run; with fail-fast off, the
          # coverage phase still warns loudly but continues so the report
          # derivation can be produced.
          "$exe" +RTS -M${testProcessMemLimit} -RTS ${coverageFlags} ${coverageJobsFlag} ${coverageRetryFlag} ${failFastFlag} \
            --mutation-suite-name ${firstSuite.suite} \
            --mutation-augmented-manifest-dir augmented
          ${extraCoverageScript}
          echo "mutation-nix: running mutations"
          mkdir -p $report
          "$exe" ${mutationFlags} \
            --mutation-augmented-manifest-dir augmented \
            ${allSuiteExeFlags} \
            --mutation-child-mem-limit ${testProcessMemLimit} \
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
