{ mkDerivation, aeson, base, http-client, http-types, lib
, monad-control, mtl, network, network-uri, path, path-io
, port-utils, sydtest, sydtest-discover, sydtest-typed-process
, sydtest-wai, transformers-base, typed-process, wai, webdriver
}:
mkDerivation {
  pname = "sydtest-webdriver";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base http-client monad-control mtl network network-uri path
    path-io port-utils sydtest sydtest-typed-process sydtest-wai
    transformers-base typed-process webdriver
  ];
  testHaskellDepends = [
    base http-types network-uri sydtest sydtest-wai wai
  ];
  testToolDepends = [ sydtest-discover ];
  description = "A webdriver companion library for sydtest";
  license = "unknown";
}
