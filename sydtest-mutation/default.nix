{ mkDerivation, aeson-pretty, base, containers, genvalidity-sydtest
, genvalidity-sydtest-aeson, lib, path, sydtest, sydtest-discover
, sydtest-mutation-runtime
}:
mkDerivation {
  pname = "sydtest-mutation";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base sydtest sydtest-mutation-runtime ];
  testHaskellDepends = [
    aeson-pretty base containers genvalidity-sydtest
    genvalidity-sydtest-aeson path sydtest sydtest-mutation-runtime
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
