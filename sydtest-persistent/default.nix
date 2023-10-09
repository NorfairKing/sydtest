{ mkDerivation, base, bytestring, lib, mtl, persistent, sydtest
, text, unliftio
}:
mkDerivation {
  pname = "sydtest-persistent";
  version = "0.0.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring mtl persistent sydtest text unliftio
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A persistent companion library for sydtest";
  license = "unknown";
}
