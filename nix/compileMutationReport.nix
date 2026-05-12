{ stdenv, lib }:

# Run one or more test suite packages in mutation mode against one or more
# manifest directories and produce a report.
#
# Always succeeds — use assertMutationScore to fail on surviving mutations.
# This separation allows inspecting the report even when mutations survive.
#
# The mutation run is driven by the first element of testPackages. All test
# suites (including the first) are passed as --mutation-suite-exe flags so the
# harness can spawn them as child processes to test each mutant.

{ name # name for the derivation
, manifests # list of 'manifest' outputs from addManifest-wrapped packages
, testPackages # list of instrumented Haskell package derivations with test suite executables in bin/
, testResourcesDir ? null # optional path to cd into before running (needed when test suites reference golden files by relative path)
}:

let
  coverageFlags = lib.concatMapStringsSep " " (m: "--mutation-coverage ${m}") manifests;
  mutationFlags = lib.concatMapStringsSep " " (m: "--mutation ${m}") manifests;

  suiteExeFlags = lib.concatMapStringsSep " "
    (pkg: "--mutation-suite-exe ${pkg.pname}=$(find ${pkg}/test -maxdepth 1 -type f | head -1)")
    testPackages;

  coveragePhaseScript = lib.concatMapStringsSep "\n"
    (pkg: ''
      echo "mutation-nix: collecting coverage for suite ${pkg.pname}"
      (
        ${lib.optionalString (testResourcesDir != null) "cd ${testResourcesDir}"}
        exe=$(find ${pkg}/test -maxdepth 1 -type f | head -1)
        # +RTS -M4g -RTS: cap the heap to 4 GB to avoid OOM in the Nix sandbox.
        "$exe" +RTS -M4g -RTS \
          ${coverageFlags} \
          --mutation-suite-name ${pkg.pname} \
          --mutation-augmented-manifest-dir augmented
      )
    '')
    testPackages;

  # The first package drives the top-level mutation run; the others are passed
  # as --mutation-suite-exe flags and spawned as child processes by the harness.
  firstPkg = builtins.head testPackages;
in
stdenv.mkDerivation {
  name = "${name}-mutation-report";

  # No source to unpack: this derivation only runs existing binaries.
  dontUnpack = true;

  buildInputs = testPackages;

  buildPhase = ''
    mkdir -p augmented
    ${coveragePhaseScript}
    echo "mutation-nix: running mutations"
    (
      ${lib.optionalString (testResourcesDir != null) "cd ${testResourcesDir}"}
      firstExe=$(find ${firstPkg}/bin -maxdepth 1 -type f | head -1)
      "$firstExe" +RTS -M4g -RTS \
        ${mutationFlags} \
        --mutation-augmented-manifest-dir augmented \
        ${suiteExeFlags} \
        --mutation-report-dir augmented
    ) | tee report.txt
  '';

  installPhase = ''
    mkdir -p $out
    cp report.txt $out/report.txt
    cp augmented/report.json $out/report.json
  '';
}
