{ mkDerivation, base, lib, process }:
mkDerivation {
  pname = "sydtest-mutation-runtime";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base process ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
