{ mkDerivation, aeson, base, bytestring, containers, genvalidity
, genvalidity-text, lib, path, path-io, QuickCheck, text
}:
mkDerivation {
  pname = "sydtest-mutation-runtime";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers genvalidity genvalidity-text path
    path-io QuickCheck text
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
