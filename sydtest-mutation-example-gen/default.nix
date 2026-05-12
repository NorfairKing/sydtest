{ mkDerivation, base, lib, QuickCheck, sydtest
, sydtest-mutation-example
}:
mkDerivation {
  pname = "sydtest-mutation-example-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base QuickCheck ];
  testHaskellDepends = [
    base QuickCheck sydtest sydtest-mutation-example
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
