{ mkDerivation, base, bytestring, containers, lib, monad-logger
, network-uri, path, path-io, persistent, postgres-options, sydtest
, sydtest-discover, sydtest-persistent-postgresql, text
, tmp-postgres, typed-process, unliftio
}:
mkDerivation {
  pname = "sydtest-sqitch-postgres";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers monad-logger network-uri path persistent
    postgres-options sydtest sydtest-persistent-postgresql text
    tmp-postgres typed-process
  ];
  testHaskellDepends = [
    base containers path path-io persistent sydtest
    sydtest-persistent-postgresql text unliftio
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A sqitch-on-PostgreSQL companion library for sydtest";
  license = "unknown";
}
