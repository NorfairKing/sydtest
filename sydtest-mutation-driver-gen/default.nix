{ mkDerivation, base, containers, genvalidity
, genvalidity-containers, genvalidity-path, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, lib, path, path-io
, QuickCheck, sydtest, sydtest-discover, sydtest-mutation-driver
, sydtest-mutation-runtime
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
    base containers genvalidity-sydtest genvalidity-sydtest-aeson path
    path-io sydtest sydtest-mutation-driver sydtest-mutation-runtime
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "Generators and tests for sydtest-mutation-driver's config types";
  license = lib.licenses.mit;
}
