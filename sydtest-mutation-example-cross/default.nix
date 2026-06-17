{ mkDerivation, base, lib }:
mkDerivation {
  pname = "sydtest-mutation-example-cross";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = "unknown";
}
