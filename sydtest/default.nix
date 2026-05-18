{ mkDerivation, async, autodocodec, base, bytestring, containers
, deepseq, dlist, fast-myers-diff, filepath, lib, list-t
, MonadRandom, mtl, opt-env-conf, path, path-io, pretty-show
, QuickCheck, quickcheck-io, random, random-shuffle, safe
, safe-coloured-text, safe-coloured-text-terminfo, stm
, stm-containers, svg-builder, sydtest-mutation-runtime, text, time
, transformers, typed-process, vector
}:
mkDerivation {
  pname = "sydtest";
  version = "0.23.1.0";
  src = ./.;
  libraryHaskellDepends = [
    async autodocodec base bytestring containers deepseq dlist
    fast-myers-diff filepath list-t MonadRandom mtl opt-env-conf path
    path-io pretty-show QuickCheck quickcheck-io random random-shuffle
    safe safe-coloured-text safe-coloured-text-terminfo stm
    stm-containers svg-builder sydtest-mutation-runtime text time
    transformers typed-process vector
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A modern testing framework for Haskell with good defaults and advanced testing features";
  license = "unknown";
}
