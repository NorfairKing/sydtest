{ mkDerivation, base, containers, lib, mtl }:
mkDerivation {
  pname = "sydtest-mutation-example";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers mtl ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = "unknown";
}
