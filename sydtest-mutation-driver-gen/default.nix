{ mkDerivation, base, bytestring, containers, genvalidity
, genvalidity-containers, genvalidity-path, genvalidity-sydtest
, genvalidity-text, lib, path, path-io, safe-coloured-text, sydtest
, sydtest-discover, sydtest-mutation-driver
, sydtest-mutation-runtime, text
}:
mkDerivation {
  pname = "sydtest-mutation-driver-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-containers genvalidity-path
    genvalidity-text sydtest-mutation-driver
  ];
  testHaskellDepends = [
    base bytestring containers genvalidity-sydtest path path-io
    safe-coloured-text sydtest sydtest-mutation-driver
    sydtest-mutation-runtime text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "Generators and tests for sydtest-mutation-driver's config types";
  license = lib.licenses.mit;
}
