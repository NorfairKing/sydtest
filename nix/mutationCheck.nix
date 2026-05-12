{ haskellPackages, pkgs }:

# Build one mutation check.
#
# - name: derivation name prefix
# - packages: list of attr names in haskellPackages for packages that have both
#             a library to instrument and a test suite to run
# - libraries: list of attr names to instrument but not run test suites for
# - tests: list of attr names to run test suites for but not instrument
# - exceptions: list of module names to skip during instrumentation
#
# The mutations are run inside the Cabal build's checkPhase of the first test
# package, so test resources (golden files, data files) are available at
# relative paths. All other test suite executables are Nix store paths.

{ name
, packages ? [ ]
, libraries ? [ ]
, tests ? [ ]
, exceptions ? [ ]
, debug ? false
, ghcMemLimit ? "16g"
}:

let
  inherit (haskellPackages.sydtest) addManifest;

  libraryPackages = packages ++ libraries;
  testPackages = packages ++ tests;

  instrumentedHaskellPackages = haskellPackages.extend (_: super:
    builtins.listToAttrs (map
      (pkg: {
        name = pkg;
        value = addManifest { inherit exceptions debug ghcMemLimit; } super.${pkg};
      })
      libraryPackages));

  # Build test packages (instrumented) with doCheck=true so the test executable
  # is compiled, then copy it to $out/bin/ in postInstall (cabal copy doesn't
  # install test executables automatically).
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

  # The first test package drives the parent mutation process (in-tree via find dist).
  firstTestPkg = builtins.head testPackages;
  extraTestPkgs = builtins.tail testPackages;

  # For extra suites, the exe is found at runtime in the store bin/.
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
in
((pkgs.haskell.lib.overrideCabal
  (pkgs.haskell.lib.dontBenchmark
    (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${firstTestPkg}))
  (_old: {
    checkPhase = ''
      exe=$(find dist -type f -executable | grep -v '\.so' | head -1)
      mkdir -p augmented
      echo "mutation-nix: collecting coverage for suite ${firstTestPkg}"
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
    postCheck = "";
  })).overrideAttrs (old: {
  buildInputs = (old.buildInputs or [ ]) ++ extraInputs;
  outputs = (old.outputs or [ "out" ]) ++ [ "report" ];
}))
