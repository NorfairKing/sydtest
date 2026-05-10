{ mkDerivation, base, directory, filepath, ghc, lib, mtl, path
, path-io, sydtest-mutation-runtime, template-haskell
}:
mkDerivation {
  pname = "sydtest-mutation-plugin";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base directory filepath ghc mtl path path-io
    sydtest-mutation-runtime template-haskell
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  license = lib.licenses.mit;
}
