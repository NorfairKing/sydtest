{ mkDerivation, async, autodocodec, autodocodec-yaml, base
, bytestring, containers, dlist, envparse, fast-myers-diff
, filepath, lib, MonadRandom, mtl, opt-env-conf
, optparse-applicative, path, path-io, pretty-show, QuickCheck
, quickcheck-io, random, random-shuffle, safe, safe-coloured-text
, safe-coloured-text-terminfo, stm, svg-builder, text, vector
}:
mkDerivation {
  pname = "sydtest";
  version = "0.15.1.3";
  src = ./.;
  libraryHaskellDepends = [
    async autodocodec autodocodec-yaml base bytestring containers dlist
    envparse fast-myers-diff filepath MonadRandom mtl opt-env-conf
    optparse-applicative path path-io pretty-show QuickCheck
    quickcheck-io random random-shuffle safe safe-coloured-text
    safe-coloured-text-terminfo stm svg-builder text vector
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A modern testing framework for Haskell with good defaults and advanced testing features";
  license = "unknown";
}
