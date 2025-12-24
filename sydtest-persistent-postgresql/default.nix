{ mkDerivation, base, bytestring, lib, monad-logger, mtl
, persistent, persistent-postgresql, postgres-options
, postgresql-simple, random, sydtest, sydtest-discover
, sydtest-persistent, text, tmp-postgres
}:
mkDerivation {
  pname = "sydtest-persistent-postgresql";
  version = "0.2.0.3";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring monad-logger mtl persistent-postgresql
    postgres-options postgresql-simple random sydtest
    sydtest-persistent text tmp-postgres
  ];
  testHaskellDepends = [ base persistent sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "An persistent-postgresql companion library for sydtest";
  license = "unknown";
}
