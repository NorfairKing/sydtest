{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, containers, fast-myers-diff, genvalidity, genvalidity-containers
, genvalidity-path, genvalidity-text, lib, path, path-io
, QuickCheck, safe-coloured-text, text, unordered-containers
, vector
}:
mkDerivation {
  pname = "sydtest-mutation-runtime";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers
    fast-myers-diff genvalidity genvalidity-containers genvalidity-path
    genvalidity-text path path-io QuickCheck safe-coloured-text text
    unordered-containers vector
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = "unknown";
}
