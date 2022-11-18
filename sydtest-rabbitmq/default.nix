{ mkDerivation, aeson, base, lib, network, path, path-io
, port-utils, sydtest, sydtest-discover, sydtest-typed-process
, text, typed-process
}:
mkDerivation {
  pname = "sydtest-rabbitmq";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base network path path-io port-utils sydtest
    sydtest-typed-process text typed-process
  ];
  testHaskellDepends = [ base sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "An rabbitmq companion library for sydtest";
  license = "unknown";
}
