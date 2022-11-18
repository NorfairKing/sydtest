{ mkDerivation, amqp, base, lib, sydtest, sydtest-discover
, sydtest-rabbitmq
}:
mkDerivation {
  pname = "sydtest-amqp";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ amqp base sydtest sydtest-rabbitmq ];
  testHaskellDepends = [ amqp base sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "An amqp companion library for sydtest";
  license = "unknown";
}
