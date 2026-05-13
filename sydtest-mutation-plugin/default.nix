{ mkDerivation, base, bytestring, containers, directory, filepath
, ghc, ghc-boot, lib, mtl, path, path-io, safe-coloured-text
, sydtest-mutation-runtime, template-haskell, text
}:
mkDerivation {
  pname = "sydtest-mutation-plugin";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers directory filepath ghc ghc-boot mtl path
    path-io safe-coloured-text sydtest-mutation-runtime
    template-haskell text
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
