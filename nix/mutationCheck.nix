{ haskellPackages, pkgs }:

# Build one mutation check.
#
# - name: derivation name prefix
# - libraryPackages: list of attr names in haskellPackages for the libraries to instrument
# - testPackage: attr name in haskellPackages for the package containing the test binary
# - testExecutableName: name of the executable inside testPackage
# - exceptions: list of module names to skip during instrumentation (applies to all libraries)
#
# The mutations are run inside the Cabal build's checkPhase, so test resources
# (golden files, data files) are available at the expected relative paths.

{ name
, libraryPackages
, testPackage
, testExecutableName
, exceptions ? [ ]
, debug ? false
, ghcMemLimit ? "16g"
}:

let
  inherit (haskellPackages.sydtest) addManifest;
  instrumentedHaskellPackages = haskellPackages.extend (_: super:
    builtins.listToAttrs (map
      (pkg: {
        name = pkg;
        value = addManifest { inherit exceptions debug ghcMemLimit; } super.${pkg};
      })
      libraryPackages));
  manifests = map (pkg: instrumentedHaskellPackages.${pkg}.manifest) libraryPackages;
  coverageFlags = pkgs.lib.concatMapStringsSep " "
    (m: "--mutation-coverage ${m}")
    manifests;
  mutationFlags = pkgs.lib.concatMapStringsSep " "
    (m: "--mutation ${m}")
    manifests;
in
(pkgs.haskell.lib.overrideCabal
  (pkgs.haskell.lib.dontBenchmark
    (pkgs.haskell.lib.doCheck instrumentedHaskellPackages.${testPackage}))
  (_old: {
    checkPhase = ''
      exe=$(find dist -name "${testExecutableName}" -type f | head -1)
      mkdir -p augmented
      echo "mutation-nix: collecting per-test coverage"
      "$exe" +RTS -M4g -RTS ${coverageFlags} --mutation-augmented-manifest-dir augmented
      echo "mutation-nix: running mutations"
      mkdir -p $report
      "$exe" ${mutationFlags} --mutation-augmented-manifest-dir augmented --mutation-child-mem-limit 4g --mutation-report-dir "$report" | tee $report/report.txt
    '';
    postCheck = "";
  })).overrideAttrs (old: {
  outputs = (old.outputs or [ "out" ]) ++ [ "report" ];
})
