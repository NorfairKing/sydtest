{ mkDerivation, base, hedis, lib, network, path, path-io
, port-utils, sydtest, sydtest-discover, sydtest-typed-process
, text, typed-process
}:
mkDerivation {
  pname = "sydtest-hedis";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base hedis network path path-io port-utils sydtest
    sydtest-typed-process text typed-process
  ];
  testHaskellDepends = [ base hedis sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "An hedis companion library for sydtest";
  license = "unknown";
}
