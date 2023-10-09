{ mkDerivation, base, lib, monad-logger, mtl, persistent
, persistent-sqlite, sydtest, sydtest-discover, sydtest-persistent
}:
mkDerivation {
  pname = "sydtest-persistent-sqlite";
  version = "0.2.0.3";
  src = ./.;
  libraryHaskellDepends = [
    base monad-logger mtl persistent persistent-sqlite sydtest
    sydtest-persistent
  ];
  testHaskellDepends = [ base persistent sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A persistent-sqlite companion library for sydtest";
  license = "unknown";
}
