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
# The returned derivation carries the building blocks as passthru attributes,
# so each piece can be built and inspected on its own:
#
# - 'passthru.report': an attrset, keyed by instrumented-library package name,
#   of per-library mutation reports ('report.txt'/'report.json' each).  The
#   mutation phase is split per library: 'passthru.report.<lib>' is the report
#   for one library, the same derivation that library's sealed check is built
#   from.  The returned derivation itself is an aggregate that symlinks every
#   per-library piece under '<lib>/', so building it builds them all.
#
# - 'passthru.coverage': a separate, much cheaper set of derivations that run
#   only the coverage phase and emit the augmented manifest ('augmented/', the
#   which-test-covers-which-mutation map) plus the per-suite test-location
#   listings ('test-locations/<suite>.json').  Coverage is gathered in one
#   derivation per test-package (so editing one package's tests only
#   invalidates that package's coverage, and the per-package runs build in
#   parallel).  None of this runs the mutation phase.  Each per-package
#   coverage is reachable as a passthru on this derivation, keyed by package
#   name ('passthru.coverage.<pkg>'), so a single package's coverage can be
#   built and inspected on its own; 'passthru.coverage' itself is an aggregate
#   view that symlinks each package's coverage under '<pkg>/'.  See the
#   'coverage' binding below.
#
# - 'passthru.diff': a 'nix run'-able diff-scoped mutation runner.  It depends
#   on the per-package coverage derivations (NOT the full report), reading them
#   directly (one '--coverage-dir' each) and unioning their augmented manifests
#   in-memory, to mutation-test only the subset
#   of mutations implied by a diff: source-file changes select the mutations
#   whose span falls in a changed hunk; test-file changes select the mutations
#   the changed tests cover.  It runs only those mutation children — no
#   compilation and no coverage phase at 'nix run' time.  Run it from inside
#   the repository working tree; by default it diffs against the merge-base
#   with the base branch (override with '--diff FILE', '--diff-stdin', or
#   '--base BRANCH').  See the 'diff' binding below.
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
# - configFile: optional path to a YAML config file consumed by the mutation
#       plugin. Defaults to null, meaning the plugin uses its built-in
#       defaults.
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
# - mutationJobs: maximum number of mutation children to run concurrently.
#       Defaults to the same value as 'coverageJobs'. The mutation phase has
#       the same contention hazard as coverage: at full core-count
#       concurrency a per-test resource (e.g. tmp-postgres) flakes, which
#       surfaces as a spuriously-failing test and therefore a FALSE KILL —
#       non-reproducible and inconsistent with a lower-volume diff-scoped
#       run. Cap it for the same reason you cap 'coverageJobs'. Pass 'null'
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

{ name ? "mutation"
, packages ? [ ]
, libraries ? [ ]
, tests ? [ ]
, needToBeLinkedAgainstMutationRuntime ? [ ]
, configFile ? null
, assertAllKilled ? true
, assertNoneUncovered ? true
, ghcMemLimit ? "16g"
  # RTS heap cap for each mutation child during the mutation phase.
  # Passed to the driver via the YAML config's 'childMemLimit'.
, testProcessMemLimit ? "4g"
, coverageJobs ? 4
  # Maximum number of mutation children to run concurrently during the
  # mutation phase. Defaults to 'coverageJobs' (the same contention cap).
, mutationJobs ? coverageJobs
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
        # Rename the instrumented-library compile derivation so the build log
        # says which phase ('instrument') and package it is, instead of the
        # bare '<pname>-<version>' the Haskell builder would give it.
        value = (addManifest
          {
            inherit configFile ghcMemLimit;
          }
          super.${pkg}).overrideAttrs (_: {
          name = "${name}-instrument-${pkg}";
        });
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
    (pkgs.haskell.lib.overrideCabal
      (pkgs.haskell.lib.dontBenchmark
        (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${pkgName}))
      (old: {
        checkPhase = "";
        postInstall = ''
          ${driver}/bin/sydtest-mutation-driver install-components \
            test-suites "${old.pname}" "$out/test"
        '';
      })).overrideAttrs (_: {
      # Distinguish the test-suite compile from the library 'instrument'
      # build above: same package, but built with --enable-tests.
      name = "${name}-testsuite-${pkgName}";
    });

  manifests = map (pkg: instrumentedHaskellPackages.${pkg}.manifest) libraryPackages;

  # Resource directory for one package: the unpacked source tree of that
  # package.  Used by the driver to 'cd' into a suite's source tree before
  # spawning it (so golden files and data files resolve via relative paths
  # just as they would during a Cabal 'checkPhase').
  #
  # The instrumented package's 'src' is an sdist tarball
  # (haskellPackages.buildFromSdist wraps it), so we have to unpack
  # it before the driver can cd into the directory.  'pkgs.srcOnly' is
  # the nixpkgs helper for exactly this: unpack a src to a derivation
  # whose $out is the resulting directory.
  unpackedSrcFor = pkgName:
    pkgs.srcOnly {
      name = "${name}-resource-${pkgName}";
      src = instrumentedHaskellPackages.${pkgName}.src;
      stdenv = pkgs.stdenvNoCC;
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

  mutationJobsFlag =
    pkgs.lib.optionalString (mutationJobs != null)
      "--mutation-jobs=${toString mutationJobs}";
  coverageRetryFlag =
    pkgs.lib.optionalString (coverageRetry != null)
      "--coverage-retry=${toString coverageRetry}";
  failFastFlag = if failFast then "--fail-fast" else "--no-fail-fast";

  # The coverage cache, split into one derivation per test-package plus a
  # cheap merge.  Both steps run ONLY the coverage phase (never the mutation
  # phase), so the cache is much cheaper than the full per-library reports
  # ('perLibraryReport' below) and is what '.diff' depends on — the diff-scoped
  # runner never needs the full mutation run to have happened.
  #
  # Splitting per test-package means editing one package's tests only
  # invalidates that package's coverage derivation, the others stay cached, and
  # the per-package coverage runs build in parallel.

  # One per-package coverage derivation: run the 'coverage' subcommand for a
  # single --suite-pkg (but against ALL --manifest dirs, so the full mutation
  # set is known), writing that package's augmented manifest to $out/augmented
  # and its suites' TestId -> source-location listings to
  # $out/test-locations/<suite>.json (a JSON array of TestLocation objects).
  #
  # The listing only walks the spec tree (no tests execute), so it is cheap; we
  # cd into the package's resource dir first so spec-definition IO ('runIO')
  # resolves relative paths the same way the driver does.
  perPackageCoverage = pkg:
    pkgs.stdenv.mkDerivation {
      name = "${name}-coverage-${pkg}";
      dontUnpack = true;
      buildInputs = [ (builtTestPkg pkg) driver ];
      nativeBuildInputs = collectedTestToolDepends;
      buildPhase = ''
        runHook preBuild

        ${driver}/bin/sydtest-mutation-driver coverage \
          ${pkgs.lib.concatStringsSep " " manifestFlags} \
          --suite-pkg=${pkg}=${builtTestPkg pkg}=${unpackedSrcFor pkg} \
          ${failFastFlag} \
          ${coverageJobsFlag} \
          ${coverageRetryFlag} \
          --mutation-augmented-manifest-dir="$out/augmented"

        mkdir -p "$out/test-locations"
        for exe in "${builtTestPkg pkg}/test/"*; do
          [ -e "$exe" ] || continue
          suite="$(basename "$exe")"
          ( cd "${unpackedSrcFor pkg}" \
            && "$exe" --mutation-coverage-list-locations ) \
            > "$out/test-locations/$suite.json"
        done

        runHook postBuild
      '';
      # buildPhase populates $out directly; there is nothing to install.
      dontInstall = true;
    };

  perPackageCoverages = map perPackageCoverage testPackages;

  # One --coverage-dir flag per per-package coverage derivation.  Both the
  # per-library report ('run') and the diff runner ('diff') hand the same set
  # of per-package coverage directories to the driver, which unions them.
  coverageDirFlags =
    pkgs.lib.concatMapStringsSep " " (cov: "--coverage-dir=${cov}") perPackageCoverages;

  # The same per-package coverage derivations, keyed by package name, so a
  # single package's coverage can be built and inspected on its own via the
  # 'coverage' derivation's per-package passthrus (e.g.
  # 'nix build .#…​.coverage.<pkg>').
  perPackageCoverageByName =
    builtins.listToAttrs (map
      (pkg: { name = pkg; value = perPackageCoverage pkg; })
      testPackages);

  # The aggregate coverage view: a cheap derivation that symlinks each
  # package's coverage under its own subdirectory ($out/<pkg> -> that
  # package's augmented/ + test-locations/).  Per-package subdirs avoid the
  # 'augmented/manifest-augmented.json' filename collision a flat join would
  # hit.  This is purely for convenient inspection ('nix build .#…​.coverage');
  # the diff runner does NOT consume it — it reads the per-package coverage
  # directories directly (one --coverage-dir each), unioning their augmented
  # manifests in-memory, so there is no merge derivation.
  #
  # The per-package coverage derivations are attached as passthrus, so
  # 'coverage' is both the aggregate view and the namespace for its
  # per-package inputs ('coverage.<pkg>').
  coverage =
    (pkgs.runCommand "${name}-coverage" { } (
      pkgs.lib.concatMapStringsSep "\n"
        (pkg: ''
          mkdir -p "$out"
          ln -s "${perPackageCoverage pkg}" "$out/${pkg}"
        '')
        testPackages
    )).overrideAttrs (old: {
      passthru = (old.passthru or { }) // perPackageCoverageByName;
    });

  # One mutation-report derivation per instrumented library.  Each runs the
  # driver's 'run' subcommand with ONLY that library's --manifest (but all
  # --suite-pkg test suites), so the expensive mutation phase is split per
  # library instead of being one run "for them all together": the per-library
  # runs build in parallel and a surviving-mutation or crash is attributed to a
  # single library rather than the whole batch.
  #
  # The instrumented test executables are shared — built once against the
  # all-instrumented overlay (every library's 'ifMutation' call sites are
  # compiled in).  Passing one library's manifest restricts which mutations the
  # driver toggles to that library, with no recompilation: the others stay at
  # baseline.
  perLibraryReport = libPkg:
    pkgs.stdenv.mkDerivation {
      name = "${name}-mutation-${libPkg}";
      dontUnpack = true;
      buildInputs = testPkgInputs ++ [ driver ];
      # Test executables spawn tools (postgresql, git, nix, ...) at
      # runtime via testToolDepends. Put the union of those deps on
      # PATH so children can find them.
      nativeBuildInputs = collectedTestToolDepends;
      # The driver writes report.txt, report.json, and every per-suite
      # *.log file directly into --out-dir.  It does NOT gather coverage here:
      # coverage is the per-package 'coverage' derivations ('perPackageCoverages',
      # the same ones '.diff' consumes), passed in via --coverage-dir.  The driver
      # unions them (which is where a suite in one package covering another
      # package's mutations is recorded) and restricts the union to this library's
      # --manifest.  Reusing them means coverage is computed once per test package
      # rather than re-run inside every per-library report.  The augmented manifest
      # it assembles lives in a workdir-local directory the driver creates itself.
      buildPhase = ''
        runHook preBuild

        ${driver}/bin/sydtest-mutation-driver run \
          --manifest=${toString instrumentedHaskellPackages.${libPkg}.manifest} \
          ${pkgs.lib.concatStringsSep " " suitePkgFlags} \
          ${coverageDirFlags} \
          --child-mem-limit=${testProcessMemLimit} \
          ${failFastFlag} \
          ${mutationJobsFlag} \
          --mutation-augmented-manifest-dir=augmented \
          --out-dir="$out"

        runHook postBuild
      '';
      # buildPhase writes report.txt/report.json into $out directly; there is
      # nothing to install.
      dontInstall = true;
    };

  # The per-library reports keyed by instrumented-library package name, exposed
  # as the '.report.<lib>' passthru so each library's report can be built and
  # inspected on its own.
  reportByLib =
    builtins.listToAttrs (map
      (libPkg: { name = libPkg; value = perLibraryReport libPkg; })
      libraryPackages);

  # An aggregate view over a per-library attrset: a cheap derivation that
  # symlinks each library's derivation under '$out/<lib>', so building it forces
  # every per-library piece while each stays reachable for inspection.
  aggregateNamed = aggName: drvsByLib:
    pkgs.runCommand aggName { } (
      pkgs.lib.concatStringsSep "\n"
        (pkgs.lib.mapAttrsToList
          (libPkg: d: ''
            mkdir -p "$out"
            ln -s "${d}" "$out/${libPkg}"
          '')
          drvsByLib)
    );

  # The diff-scoped runner: 'nix run' this to mutation-test only the subset
  # of mutations implied by a diff (default: 'git diff' against the
  # merge-base with the base branch; override with '--diff FILE',
  # '--diff-stdin', or '--base BRANCH').
  #
  # It depends only on the cheap 'coverage' derivation (the augmented
  # manifest + per-suite test-location listings) and the cached instrumented
  # test exes — NOT on the full mutation report.  No compilation and no
  # coverage phase run at 'nix run' time; only the selected mutation children
  # execute.  It must be run from inside the repository working tree so the
  # default 'git' diff and the suites' relative resource paths resolve.
  #
  # Extra arguments are forwarded to 'sydtest-mutation-driver diff' (e.g.
  # '--diff FILE', '--base BRANCH', '--no-fail-fast').  The report output
  # directory defaults to a fresh temp dir; override it with the
  # MUTATION_DIFF_OUT_DIR environment variable or by passing your own
  # '--out-dir' (in which case the wrapper does not inject its default, since
  # the driver rejects a duplicate '--out-dir').
  diff = pkgs.writeShellApplication {
    name = "${name}-mutation-diff";
    runtimeInputs = [ driver pkgs.git pkgs.coreutils ] ++ collectedTestToolDepends;
    text = ''
      # Only inject a default --out-dir when the caller didn't pass one.
      # The driver itself prints the final PASS/FAIL summary and the
      # report-file paths, so the wrapper does not echo anything else
      # — that would push the driver's summary off the bottom of the
      # build log.
      out_dir_args=()
      case " $* " in
        *" --out-dir "* | *" --out-dir="*) ;;
        *) out_dir_args=(--out-dir "''${MUTATION_DIFF_OUT_DIR:-$(mktemp -d)}") ;;
      esac
      sydtest-mutation-driver diff \
        ${pkgs.lib.concatStringsSep " " suitePkgFlags} \
        ${coverageDirFlags} \
        --child-mem-limit=${testProcessMemLimit} \
        ${mutationJobsFlag} \
        "''${out_dir_args[@]}" \
        "$@"
    '';
  };

  # One assertMutationScore check per library, keyed by library name.  Each
  # fails the build if that library has surviving (or, by default, uncovered)
  # mutations; the aggregate below fails if any per-library check fails.
  checkByLib =
    builtins.mapAttrs
      (libPkg: rep: assertMutationScore {
        name = "${name}-check-${libPkg}";
        inherit assertNoneUncovered;
        report = rep;
      })
      reportByLib;

  # Attach the building blocks as passthrus on whichever derivation we return,
  # so a single 'mutationCheck { ... }' call yields not just the sealed
  # check/report but every intermediate piece, each buildable and inspectable
  # on its own:
  #
  # - '.diff'              the diff-scoped runner (nix run)
  # - '.report'           an attrset keyed by instrumented-library name; each
  #                       '.report.<lib>' is that library's mutation report
  #                       (report.txt/report.json)
  # - '.coverage'         the merged coverage (augmented/ + test-locations/);
  #                       its own per-package passthrus ('.coverage.<pkg>')
  #                       give each test-package's coverage on its own
  withPassthru = drv': drv'.overrideAttrs (old: {
    passthru = (old.passthru or { }) // {
      inherit diff coverage;
      report = reportByLib;
    };
  });
in
if assertAllKilled
then withPassthru (aggregateNamed "${name}-mutation-check" checkByLib)
else withPassthru (aggregateNamed "${name}-mutation-report" reportByLib)
