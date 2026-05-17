{ mkDerivation, base, lib, monad-logger, path, path-io, persistent
, sydtest, sydtest-discover, sydtest-persistent
, sydtest-persistent-postgresql, sydtest-sqitch-postgres, text
}:
mkDerivation {
  pname = "sydtest-sqitch-postgres-persistent";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base monad-logger persistent sydtest sydtest-persistent
    sydtest-persistent-postgresql sydtest-sqitch-postgres text
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
