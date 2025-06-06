{ mkDerivation, base, hspec, hspec-core, lib, mtl, QuickCheck
, safe-coloured-text, stm, sydtest, sydtest-discover, sydtest-test
, text
}:
mkDerivation {
  pname = "sydtest-hspec";
  version = "0.4.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base hspec-core mtl QuickCheck stm sydtest
  ];
  testHaskellDepends = [
    base hspec safe-coloured-text stm sydtest sydtest-test text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "An Hspec companion library for sydtest";
  license = "unknown";
}
