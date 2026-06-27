{ mkDerivation, base, bytestring, containers, lib, monad-logger
, network-uri, path, path-io, persistent, postgres-options, random
, sydtest, sydtest-discover, sydtest-persistent-postgresql, text
, typed-process, unliftio
}:
mkDerivation {
  pname = "sydtest-sqitch-postgres";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers monad-logger network-uri path persistent
    postgres-options random sydtest sydtest-persistent-postgresql text
    typed-process
  ];
  testHaskellDepends = [
    base bytestring path path-io sydtest sydtest-persistent-postgresql
    text unliftio
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A sqitch-on-PostgreSQL companion library for sydtest";
  license = "unknown";
}
