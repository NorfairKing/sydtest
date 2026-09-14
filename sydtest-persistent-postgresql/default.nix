{ mkDerivation, base, directory, filepath, lib, monad-logger, mtl
, persistent, persistent-postgresql, postgres-options
, postgresql-simple, random, sydtest, sydtest-discover
, sydtest-persistent, temporary, text, time, tmp-postgres
, typed-process
}:
mkDerivation {
  pname = "sydtest-persistent-postgresql";
  version = "0.6.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base directory filepath monad-logger mtl persistent-postgresql
    postgres-options postgresql-simple random sydtest
    sydtest-persistent temporary text time tmp-postgres typed-process
  ];
  testHaskellDepends = [
    base persistent postgresql-simple sydtest tmp-postgres
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "An persistent-postgresql companion library for sydtest";
  license = "unknown";
}
