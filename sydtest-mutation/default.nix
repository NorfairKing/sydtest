{ mkDerivation, base, genvalidity, genvalidity-sydtest
, genvalidity-sydtest-aeson, lib, QuickCheck, sydtest
, sydtest-discover, sydtest-mutation-runtime, text, validity-text
}:
mkDerivation {
  pname = "sydtest-mutation";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base sydtest sydtest-mutation-runtime ];
  testHaskellDepends = [
    base genvalidity genvalidity-sydtest genvalidity-sydtest-aeson
    QuickCheck sydtest sydtest-mutation-runtime text validity-text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
