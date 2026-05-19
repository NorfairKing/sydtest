{ mkDerivation, aeson, async, autodocodec, base, bytestring, Cabal
, containers, directory, lib, opt-env-conf, path, path-io
, safe-coloured-text, stm, sydtest, sydtest-mutation-runtime, text
, typed-process
}:
mkDerivation {
  pname = "sydtest-mutation-driver";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async autodocodec base bytestring Cabal containers directory
    opt-env-conf path path-io safe-coloured-text stm sydtest
    sydtest-mutation-runtime text typed-process
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "Out-of-process mutation testing driver for sydtest";
  license = lib.licenses.mit;
  mainProgram = "sydtest-mutation-driver";
}
