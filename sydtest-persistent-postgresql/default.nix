{ mkDerivation, base, lib, monad-logger, mtl, path, persistent
, persistent-postgresql, sydtest, sydtest-discover
, sydtest-persistent, tmp-postgres
}:
mkDerivation {
  pname = "sydtest-persistent-postgresql";
  version = "0.2.0.3";
  src = ./.;
  libraryHaskellDepends = [
    base monad-logger mtl path persistent-postgresql sydtest
    sydtest-persistent tmp-postgres
  ];
  testHaskellDepends = [ base persistent sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "An persistent-postgresql companion library for sydtest";
  license = "unknown";
}
