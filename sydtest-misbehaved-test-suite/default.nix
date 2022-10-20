{ mkDerivation, base, lib, QuickCheck, sydtest }:
mkDerivation {
  pname = "sydtest-misbehaved-test-suite";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base QuickCheck sydtest ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "sydtest-misbehaved-test-suite-exe";
}
