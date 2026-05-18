{ mkDerivation, base, Cabal, lib }:
mkDerivation {
  pname = "sydtest-cabal-components";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base Cabal ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "Print executable or test-suite component names declared in a Cabal file";
  license = lib.licenses.mit;
  mainProgram = "sydtest-cabal-components";
}
