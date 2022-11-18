{ mkDerivation, autodocodec, autodocodec-yaml, base, lib, sydtest
, sydtest-discover, text
}:
mkDerivation {
  pname = "sydtest-autodocodec";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    autodocodec autodocodec-yaml base sydtest text
  ];
  testHaskellDepends = [ autodocodec base sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "An autodocodec companion library for sydtest";
  license = "unknown";
}
