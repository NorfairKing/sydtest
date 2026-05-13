{ mkDerivation, base, lib, sydtest, sydtest-discover }:
mkDerivation {
  pname = "sydtest-mutation-example";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
