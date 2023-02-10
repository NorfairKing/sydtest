{ mkDerivation, base, bytestring, http-client, http-types, lib, mtl
, network-uri, path, path-io, sydtest, sydtest-discover
, sydtest-wai, sydtest-webdriver, sydtest-yesod, text, webdriver
, yesod
}:
mkDerivation {
  pname = "sydtest-webdriver-yesod";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring http-client http-types mtl network-uri sydtest
    sydtest-wai sydtest-webdriver sydtest-yesod text webdriver yesod
  ];
  testHaskellDepends = [
    base path path-io sydtest sydtest-webdriver yesod
  ];
  testToolDepends = [ sydtest-discover ];
  description = "A webdriver+yesod companion library for sydtest";
  license = "unknown";
}
