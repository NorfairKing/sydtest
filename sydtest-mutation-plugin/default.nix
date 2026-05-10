{ mkDerivation, aeson, base, bytestring, ghc, lib, mtl, path
, path-io, sydtest-mutation-runtime
}:
mkDerivation {
  pname = "sydtest-mutation-plugin";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring ghc mtl path path-io sydtest-mutation-runtime
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
