{ mkDerivation, base, bytestring, fast-myers-diff, lib, path
, path-io, QuickCheck, random, safe-coloured-text, stm, sydtest
, sydtest-discover, text, vector
}:
mkDerivation {
  pname = "sydtest-test";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring fast-myers-diff path path-io QuickCheck random
    safe-coloured-text stm sydtest text vector
  ];
  testToolDepends = [ sydtest-discover ];
  doHaddock = false;
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
