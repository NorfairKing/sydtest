{ mkDerivation, base, bytestring, junit-xml, lib, path, path-io
, sydtest, sydtest-discover, sydtest-test, text
}:
mkDerivation {
  pname = "sydtest-junit";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base junit-xml sydtest text ];
  testHaskellDepends = [
    base bytestring junit-xml path path-io sydtest sydtest-test
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "An JUnit XML companion library for sydtest";
  license = "unknown";
}
