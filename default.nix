let
  sources = import ./nix/sources.nix;
  haskellNix = import sources."haskell.nix" { };
  pkgsf = import sources.nixpkgs;
  pkgs = pkgsf haskellNix.nixpkgsArgs;
in
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "sydtest-project";
    src = ./.;
  };
  modules = [{
    packages.sydtest-hedis = {
      components.tests.sydtest-hedis-test = {
        build-tools = [
          pkgs.redis
        ];
      };
    };
    packages.sydtest-mongo = {
      components.tests.sydtest-mongo-test = {
        build-tools = [
          pkgs.mongodb
        ];
      };
    };
    packages.sydtest-amqp = {
      # Turn off testing because erlang fails to start.
      flags.sydtest_integration_tests = false;
      components.tests.sydtest-amqp-test = {
        build-tools = [
          pkgs.rabbitmq-server
        ];
      };
    };
  }];
}
