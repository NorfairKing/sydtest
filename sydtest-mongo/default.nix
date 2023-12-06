{ mkDerivation, base, binary, bson, bytestring, lib, mongoDB
, network, path, path-io, port-utils, process, sydtest
, sydtest-process, text, yaml
}:
mkDerivation {
  pname = "sydtest-mongo";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bson bytestring mongoDB network path path-io port-utils
    process sydtest sydtest-process text yaml
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "An mongoDB companion library for sydtest";
  license = "unknown";
}
