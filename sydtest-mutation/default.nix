{ mkDerivation, base, genvalidity, genvalidity-sydtest, lib
, QuickCheck, sydtest, sydtest-discover, sydtest-mutation-runtime
, text, validity-text
}:
mkDerivation {
  pname = "sydtest-mutation";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base sydtest sydtest-mutation-runtime ];
  testHaskellDepends = [
    base genvalidity genvalidity-sydtest QuickCheck sydtest text
    validity-text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
