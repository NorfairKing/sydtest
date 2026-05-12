{ mkDerivation, base, genvalidity-sydtest
, genvalidity-sydtest-aeson, lib, sydtest, sydtest-discover
, sydtest-mutation-runtime
}:
mkDerivation {
  pname = "sydtest-mutation";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base sydtest sydtest-mutation-runtime ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-sydtest-aeson sydtest
    sydtest-mutation-runtime
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
