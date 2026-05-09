{ mkDerivation, base, ghc, lib, mtl, sydtest-mutation-runtime }:
mkDerivation {
  pname = "sydtest-mutation-plugin";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ghc mtl sydtest-mutation-runtime ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
