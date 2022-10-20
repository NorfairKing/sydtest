{ mkDerivation, base, bytestring, lib, sydtest, sydtest-discover
, typed-process
}:
mkDerivation {
  pname = "sydtest-typed-process";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base sydtest typed-process ];
  testHaskellDepends = [ base bytestring sydtest typed-process ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A typed-process companion library for sydtest";
  license = "unknown";
}
