{ mkDerivation, aeson, aeson-pretty, async, base, bytestring
, containers, genvalidity-sydtest, genvalidity-sydtest-aeson, lib
, path, path-io, sydtest, sydtest-discover
, sydtest-mutation-runtime, text
}:
mkDerivation {
  pname = "sydtest-mutation";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base sydtest sydtest-mutation-runtime ];
  testHaskellDepends = [
    aeson aeson-pretty async base bytestring containers
    genvalidity-sydtest genvalidity-sydtest-aeson path path-io sydtest
    sydtest-mutation-runtime text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = "unknown";
}
