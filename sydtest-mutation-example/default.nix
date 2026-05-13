{ mkDerivation, base, lib, mtl }:
mkDerivation {
  pname = "sydtest-mutation-example";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base mtl ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
