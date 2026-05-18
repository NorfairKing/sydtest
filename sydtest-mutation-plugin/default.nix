{ mkDerivation, aeson, autodocodec, base, bytestring, containers
, directory, filepath, ghc, ghc-boot, lib, mtl, path, path-io, stm
, stm-containers, sydtest, sydtest-discover
, sydtest-mutation-runtime, template-haskell, text, yaml
}:
mkDerivation {
  pname = "sydtest-mutation-plugin";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring containers directory filepath ghc
    ghc-boot mtl path path-io stm stm-containers
    sydtest-mutation-runtime template-haskell text yaml
  ];
  testHaskellDepends = [ base containers ghc sydtest text ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
