{ mkDerivation, async, base, bytestring, fast-myers-diff, lib
, opt-env-conf-test, path, path-io, QuickCheck, random
, safe-coloured-text, stm, sydtest, sydtest-discover, text, time
, vector
}:
mkDerivation {
  pname = "sydtest-test";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    async base bytestring fast-myers-diff opt-env-conf-test path
    path-io QuickCheck random safe-coloured-text stm sydtest text time
    vector
  ];
  testToolDepends = [ sydtest-discover ];
  doHaddock = false;
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
