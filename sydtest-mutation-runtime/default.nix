{ mkDerivation, base, lib }:
mkDerivation {
  pname = "sydtest-mutation-runtime";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
