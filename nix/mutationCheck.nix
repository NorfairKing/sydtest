{ haskellPackages, pkgs }:

# Build one mutation check.
#
# Returns an attrset with:
# - report: derivation containing report.txt and report.json; succeeds as long
#           as all test suites complete without crashing
# - check: derivation that fails the build if any mutations survive (only
#          present when assertAllKilled = true, which is the default)
#
# Arguments:
# - name: derivation name prefix
# - packages: attr names in haskellPackages for packages that have both
#             a library to instrument and a test suite to run
# - libraries: attr names to instrument but not run test suites for
# - tests: attr names whose test suites to run but not instrument
# - exceptions: module names to skip during instrumentation
# - disabledMutations: mutation type names to disable globally
# - assertAllKilled: add a 'check' output that fails if any mutations survive (default: true)
# - assertNoneUncovered: also fail the 'check' output if any mutations are uncovered (default: true)
# - debug: print each mutation site as it is recorded (for debugging the plugin)
# - ghcMemLimit: RTS heap limit for GHC during instrumented compilation
#
# The mutations are run inside the Cabal build's checkPhase of the first test
# package, so test resources (golden files, data files) are available at
# relative paths. All other test suite executables are referenced by Nix store
# paths baked in at evaluation time.

{ name
, packages ? [ ]
, libraries ? [ ]
, tests ? [ ]
, exceptions ? [ ]
, disabledMutations ? [ ]
, assertAllKilled ? true
, assertNoneUncovered ? true
, debug ? false
, ghcMemLimit ? "16g"
}:

let
  inherit (haskellPackages.sydtest) addManifest assertMutationScore;

  libraryPackages = packages ++ libraries;
  testPackages = packages ++ tests;

  instrumentedHaskellPackages = haskellPackages.extend (_: super:
    builtins.listToAttrs (map
      (pkg: {
        name = pkg;
        value = addManifest { inherit exceptions disabledMutations debug ghcMemLimit; } super.${pkg};
      })
      libraryPackages));

  # Build a test package with doCheck=true so Cabal compiles the test
  # executable, then copy it to $out/test/ in postInstall. Cabal's 'copy'
  # command only installs library and executable components, not test suites,
  # so we copy them manually from dist/build/.
  # checkPhase is cleared so the tests are not run during this build; they are
  # run later in the mutation checkPhase of the first test package.
  builtTestPkg = pkg:
    pkgs.haskell.lib.overrideCabal
      (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${pkg})
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
  mutationFlags = pkgs.lib.concatMapStringsSep " "
    (m: "--mutation ${m}")
    manifests;

  # The first test package is the one whose Cabal checkPhase we hijack to run
  # the mutation harness. Its executable is found at runtime via `find dist`
  # inside the build sandbox, so golden files and other relative paths work.
  firstTestPkg = builtins.head testPackages;
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
      "$storeExe" +RTS -M4g -RTS ${coverageFlags} \
        --mutation-suite-name ${pkg} \
        --mutation-augmented-manifest-dir augmented
    '')
    extraTestPkgs;

  extraInputs = map builtTestPkg extraTestPkgs;

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
          "$exe" +RTS -M4g -RTS ${coverageFlags} \
            --mutation-suite-name ${firstTestPkg} \
            --mutation-augmented-manifest-dir augmented
          ${extraCoverageScript}
          echo "mutation-nix: running mutations"
          mkdir -p $report
          "$exe" ${mutationFlags} \
            --mutation-augmented-manifest-dir augmented \
            ${allSuiteExeFlags} \
            --mutation-child-mem-limit 4g \
            --mutation-report-dir "$report" | tee $report/report.txt
        '';
        # Suppress the default postCheck phase so it does not re-run the test
        # suite after our mutation checkPhase has already finished.
        postCheck = "";
      })).overrideAttrs (old: {
      # extraInputs are the extra test package derivations; adding them to
      # buildInputs ensures they are present in the sandbox PATH during checkPhase.
      buildInputs = (old.buildInputs or [ ]) ++ extraInputs;
      # Add a 'report' output so callers can access report.txt and report.json
      # without having to build the full 'out' derivation.
      outputs = (old.outputs or [ "out" ]) ++ [ "report" ];
    });
in
{
  report = drv.report;
} // (if assertAllKilled then
  { check = assertMutationScore { inherit name assertNoneUncovered; report = drv.report; }; }
else
  { })
