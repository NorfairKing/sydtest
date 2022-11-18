{ mkDerivation, base, http-client, lib, servant-client
, servant-server, stm, sydtest, sydtest-discover, sydtest-wai
}:
mkDerivation {
  pname = "sydtest-servant";
  version = "0.2.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base http-client servant-client servant-server sydtest sydtest-wai
  ];
  testHaskellDepends = [
    base servant-client servant-server stm sydtest sydtest-wai
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A servant companion library for sydtest";
  license = "unknown";
}
