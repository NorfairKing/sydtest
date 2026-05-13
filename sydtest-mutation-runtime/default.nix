{ mkDerivation, aeson, autodocodec, base, bytestring, containers
, fast-myers-diff, genvalidity, genvalidity-containers
, genvalidity-path, genvalidity-text, lib, path, path-io
, QuickCheck, text
}:
mkDerivation {
  pname = "sydtest-mutation-runtime";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring containers fast-myers-diff
    genvalidity genvalidity-containers genvalidity-path
    genvalidity-text path path-io QuickCheck text
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
