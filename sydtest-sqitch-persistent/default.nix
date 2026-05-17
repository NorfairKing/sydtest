{ mkDerivation, base, lib, monad-logger, path, path-io, persistent
, sydtest, sydtest-discover, sydtest-persistent
, sydtest-persistent-postgresql, sydtest-sqitch, text
}:
mkDerivation {
  pname = "sydtest-sqitch-persistent";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base monad-logger persistent sydtest sydtest-persistent
    sydtest-persistent-postgresql sydtest-sqitch text
  ];
  testHaskellDepends = [
    base path path-io persistent sydtest sydtest-persistent-postgresql
    sydtest-sqitch text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A sqitch+persistent companion library for sydtest";
  license = "unknown";
}
