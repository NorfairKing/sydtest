{ mkDerivation, async, base, bytestring, containers, directory
, fast-myers-diff, lib, mtl, opt-env-conf-test, path, path-io
, process, QuickCheck, random, safe-coloured-text, stm, sydtest
, sydtest-ctrl-c-suite, sydtest-discover, sydtest-mutation-runtime
, text, time, vector
}:
mkDerivation {
  pname = "sydtest-test";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base sydtest ];
  testHaskellDepends = [
    async base bytestring containers directory fast-myers-diff mtl
    opt-env-conf-test path path-io process QuickCheck random
    safe-coloured-text stm sydtest sydtest-mutation-runtime text time
    vector
  ];
  testToolDepends = [ sydtest-ctrl-c-suite sydtest-discover ];
  doHaddock = false;
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = "unknown";
  mainProgram = "sydtest-ctrl-c-suite";
}
