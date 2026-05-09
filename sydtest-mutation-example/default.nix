{ mkDerivation, base, lib, sydtest }:
mkDerivation {
  pname = "sydtest-mutation-example";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base sydtest ];
  testHaskellDepends = [ base sydtest ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
