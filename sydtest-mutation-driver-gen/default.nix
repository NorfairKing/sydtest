{ mkDerivation, base, genvalidity, genvalidity-containers
, genvalidity-path, genvalidity-sydtest, genvalidity-sydtest-aeson
, genvalidity-text, lib, QuickCheck, sydtest, sydtest-discover
, sydtest-mutation-driver
}:
mkDerivation {
  pname = "sydtest-mutation-driver-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-containers genvalidity-path
    genvalidity-text QuickCheck sydtest-mutation-driver
  ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-sydtest-aeson sydtest
    sydtest-mutation-driver
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "Generators and tests for sydtest-mutation-driver's config types";
  license = lib.licenses.mit;
}
