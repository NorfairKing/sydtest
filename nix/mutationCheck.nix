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
# - testProcessMemLimit: RTS heap limit for each mutation child during the
#       mutation phase (default: "4g")
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
# The mutation driver runs out-of-tree in this build's checkPhase. Test
# resource directories are point-of-reference for each suite — the driver
# 'cd's into each suite's source-tree path before spawning it as a child
# so golden files and data files resolve via the same relative paths Cabal
# would use during a 'checkPhase'. All test suite executables are
# referenced by Nix store paths baked in at evaluation time.
#
# Executable and test-suite component names are NOT enumerated by the
# caller, nor discovered from the source tree at evaluation time. Instead,
# they are discovered at build time inside each derivation by running the
# driver's 'list-components' subcommand (which uses the 'Cabal' library)
# against the package's <pname>.cabal file. The driver's YAML config is
# likewise assembled at build time via 'jq'.

{ name
, packages ? [ ]
, libraries ? [ ]
, tests ? [ ]
, needToBeLinkedAgainstMutationRuntime ? [ ]
, config ? { }
, assertAllKilled ? true
, assertNoneUncovered ? true
, ghcMemLimit ? "16g"
  # RTS heap cap for each mutation child during the mutation phase.
  # Passed to the driver via the YAML config's 'childMemLimit'.
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
  driver = haskellPackages.sydtest-mutation-driver;

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
  # they are run later in the driver derivation.  dontBenchmark: avoid
  # building benchmark executables that would also end up under
  # dist/build/.
  #
  # Test-suite component names are discovered at build time by parsing the
  # package's cabal file with the driver's list-components subcommand.
  builtTestPkg = pkgName:
    pkgs.haskell.lib.overrideCabal
      (pkgs.haskell.lib.dontBenchmark
        (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${pkgName}))
      (old: {
        checkPhase = "";
        postInstall = ''
          ${driver}/bin/sydtest-mutation-driver install-components \
            test-suites "${old.pname}" "$out/test"
        '';
      });

  manifests = map (pkg: instrumentedHaskellPackages.${pkg}.manifest) libraryPackages;

  # Resource directory for one package: the unpacked source tree of that
  # package.  Used by the driver to 'cd' into a suite's source tree before
  # spawning it (so golden files and data files resolve via relative paths
  # just as they would during a Cabal 'checkPhase').
  #
  # The instrumented package's 'src' is an sdist tarball
  # (haskellPackages.buildFromSdist wraps it).  We unpack each tarball
  # once into a per-package store path that the driver can 'cd' into.
  unpackedSrcFor = pkgName:
    pkgs.stdenv.mkDerivation {
      name = "${pkgName}-resource-dir";
      src = instrumentedHaskellPackages.${pkgName}.src;
      nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ];
      dontConfigure = true;
      dontBuild = true;
      installPhase = ''
        mkdir -p $out
        cp -r ./. $out/
      '';
    };

  # One built test-package derivation per test-package the caller asked
  # for. Each contributes zero-or-more test executables under @$out/test@.
  testPkgInputs = map builtTestPkg testPackages;

  # Pull testToolDepends from every test package so they are visible to
  # the driver's child processes. Each package's own checkPhase is
  # cleared, so we cannot rely on per-package testToolDepends — they only
  # fire during that package's own check. We need them on PATH here.
  # 'getCabalDeps.testToolDepends' is provided by
  # haskellPackages.generic-builder whenever doCheck = true; we apply
  # doCheck before reading it.
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

  # Flag arrays passed to the driver: one --manifest per instrumented
  # library manifest, and one --suite-pkg per declared test-package.
  # The driver walks each --suite-pkg's <root>/test/* at run time to
  # discover installed test-suite executables.
  manifestFlags = map (m: "--manifest=${toString m}") manifests;
  suitePkgFlags = map
    (pkg: "--suite-pkg=${pkg}=${builtTestPkg pkg}=${unpackedSrcFor pkg}")
    testPackages;
  coverageJobsFlag =
    pkgs.lib.optionalString (coverageJobs != null)
      "--coverage-jobs=${toString coverageJobs}";
  coverageRetryFlag =
    pkgs.lib.optionalString (coverageRetry != null)
      "--coverage-retry=${toString coverageRetry}";
  failFastFlag = if failFast then "--fail-fast" else "--no-fail-fast";

  drv =
    pkgs.stdenv.mkDerivation {
      name = "${name}-mutation-report";
      dontUnpack = true;
      buildInputs = testPkgInputs ++ [ driver ];
      # Test executables spawn tools (postgresql, git, nix, ...) at
      # runtime via testToolDepends. Put the union of those deps on
      # PATH so children can find them.
      nativeBuildInputs = collectedTestToolDepends;
      # The driver writes report.txt, report.json, and every per-suite
      # *.log file directly into --out-dir.  The augmented manifest is
      # an intermediate artefact that doesn't belong in $out, so it
      # lives in a workdir-local directory the driver creates itself.
      buildPhase = ''
        runHook preBuild

        ${driver}/bin/sydtest-mutation-driver run \
          ${pkgs.lib.concatStringsSep " " manifestFlags} \
          ${pkgs.lib.concatStringsSep " " suitePkgFlags} \
          --child-mem-limit=${testProcessMemLimit} \
          ${failFastFlag} \
          ${coverageJobsFlag} \
          ${coverageRetryFlag} \
          --mutation-augmented-manifest-dir=augmented \
          --out-dir="$out"

        runHook postBuild
      '';
      installPhase = ''
        runHook preInstall
        runHook postInstall
      '';
    };

  report = drv;
  check = assertMutationScore { inherit name assertNoneUncovered report; };
in
if assertAllKilled
then check
else report
