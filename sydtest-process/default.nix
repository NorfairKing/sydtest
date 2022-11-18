{ mkDerivation, base, bytestring, lib, process, sydtest
, sydtest-discover
}:
mkDerivation {
  pname = "sydtest-process";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base process sydtest ];
  testHaskellDepends = [ base bytestring process sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A typed-process companion library for sydtest";
  license = "unknown";
}
