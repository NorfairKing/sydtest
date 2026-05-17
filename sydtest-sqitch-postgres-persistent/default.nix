{ mkDerivation, base, lib, monad-logger, path, path-io, persistent
, postgres-options, sydtest, sydtest-discover, sydtest-persistent
, sydtest-persistent-postgresql, sydtest-sqitch-postgres, text
}:
mkDerivation {
  pname = "sydtest-sqitch-postgres-persistent";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base monad-logger persistent postgres-options sydtest
    sydtest-persistent sydtest-persistent-postgresql
    sydtest-sqitch-postgres
  ];
  testHaskellDepends = [
    base path path-io persistent sydtest sydtest-persistent-postgresql
    sydtest-sqitch-postgres text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A sqitch-on-PostgreSQL + persistent companion library for sydtest";
  license = "unknown";
}
