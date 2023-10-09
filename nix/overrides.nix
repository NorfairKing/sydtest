{ lib
, haskell
, libredirect
, rabbitmq-server
, redis
, postgresql
, mongodb
, iana-etc
, chromedriver
, chromium
, selenium-server-standalone
, symlinkJoin
, callPackage
, ...
}:
self: super:
with lib;
with haskell.lib;

let
  sydtestPkg =
    name:
    buildFromSdist (
      overrideCabal (self.callPackage (../${name}) { })
        (old: {
          doBenchmark = true;
          configureFlags = (old.configureFlags or [ ]) ++ [
            # Optimisations
            "--ghc-options=-O2"
            # Extra warnings
            "--ghc-options=-Wall"
            "--ghc-options=-Wincomplete-uni-patterns"
            "--ghc-options=-Wincomplete-record-updates"
            "--ghc-options=-Wpartial-fields"
            "--ghc-options=-Widentities"
            "--ghc-options=-Wredundant-constraints"
            "--ghc-options=-Wcpp-undef"
            "--ghc-options=-Wunused-packages"
            "--ghc-options=-Werror"
            "--ghc-options=-Wno-deprecations"
          ];
          # Ugly hack because we can't just add flags to the 'test' invocation.
          # Show test output as we go, instead of all at once afterwards.
          testTarget = (old.testTarget or "") + " --show-details=direct";
        })
    );

  enableMongo = haskellPkg: overrideCabal haskellPkg (old: {
    buildDepends = (old.buildDepends or [ ]) ++ [ mongodb ];
    # The mongodb library uses network-bsd's function getProtocolByName
    # to lookup the port that corresponds to the tcp protocol:
    # https://hackage.haskell.org/package/network-bsd-2.8.1.0/docs/Network-BSD.html#v:getProtocolByName
    # This consults a system database that is in /etc/protocols, see
    # https://linux.die.net/man/3/getprotobyname and
    # https://linux.die.net/man/5/protocols
    #
    # However, this file doesn't exist in the nix sandbox, so we need to
    # somehowe make the test suite think that it does.
    #
    # There's no way to put something at /etc/protocols in the test suite,
    # but we can use LD_PRELOAD to use libredirect to replace the openat
    # glibc calls that the test suite makes by openat calls that look for a
    # different filename.
    #
    # This mapping from expected filename to actual filename is given in a
    # NIX_REDIRECTS environment variable, and the /etc/protocols file that
    # we want to use is in iana-etc.
    preCheck = (old.preCheck or "") + ''
      export NIX_REDIRECTS=/etc/protocols=${iana-etc}/etc/protocols
      export LD_PRELOAD=${libredirect}/lib/libredirect.so
    '';
    postCheck = (old.postCheck or "") + ''
      unset NIX_REDIRECTS LD_PRELOAD
    '';
  });

  fontsConfig = callPackage ./fonts-conf.nix { };
  setupFontsConfigScript = ''
    export FONTCONFIG_SYSROOT=${fontsConfig}
  '';
  enableWebdriver = haskellPkg: overrideCabal haskellPkg (old:
    let
      webdriverDeps = [
        chromedriver
        chromium
        selenium-server-standalone
      ];
    in
    {
      testDepends = (old.testDepends or [ ]) ++ webdriverDeps;
      preConfigure = (old.preConfigure or "") + setupFontsConfigScript;
      passthru = (old.passthru or { }) // { inherit webdriverDeps; };
    });

  sydtestPackages =
    {
      "sydtest" = overrideCabal (sydtestPkg "sydtest") (old: {
        # We turn off tests on other versions because they generate different
        # random data and/or add different data to the callstack.
        # This makes the output tests fail.
        doCheck = self.ghc.version == "9.2.8";
      });
      "sydtest-aeson" = sydtestPkg "sydtest-aeson";
      "sydtest-autodocodec" = sydtestPkg "sydtest-autodocodec";
      "sydtest-discover" = sydtestPkg "sydtest-discover";
      "sydtest-hedgehog" = sydtestPkg "sydtest-hedgehog";
      "sydtest-hspec" = sydtestPkg "sydtest-hspec";
      "sydtest-persistent" = sydtestPkg "sydtest-persistent";
      "sydtest-persistent-sqlite" = sydtestPkg "sydtest-persistent-sqlite";
      "sydtest-process" = sydtestPkg "sydtest-process";
      "sydtest-servant" = sydtestPkg "sydtest-servant";
      "sydtest-typed-process" = sydtestPkg "sydtest-typed-process";
      "sydtest-wai" = sydtestPkg "sydtest-wai";
      "sydtest-yesod" = sydtestPkg "sydtest-yesod";
      "sydtest-amqp" = overrideCabal (sydtestPkg "sydtest-amqp") (old: {
        testDepends = (old.testDepends or [ ]) ++ [ rabbitmq-server ];
        # Turn off testing because it hangs for unknown reasons?
        doCheck = false;
      });
      "sydtest-rabbitmq" = overrideCabal (sydtestPkg "sydtest-rabbitmq") (old: {
        testDepends = (old.testDepends or [ ]) ++ [ rabbitmq-server ];
        # Turn off testing because it hangs for unknown reasons?
        doCheck = false;
      });
      "sydtest-hedis" = overrideCabal (sydtestPkg "sydtest-hedis") (old: {
        testDepends = (old.testDepends or [ ]) ++ [ redis ];
      });
      "sydtest-persistent-postgresql" = overrideCabal (sydtestPkg "sydtest-persistent-postgresql") (old: {
        testDepends = (old.testDepends or [ ]) ++ [ postgresql ];
        # Turn off testing there's something wrong with a gclib version on
        # older nixpkgs versions?
        doCheck = false;
      });
      "sydtest-webdriver" = (enableWebdriver (sydtestPkg "sydtest-webdriver")).overrideAttrs (old: {
        passthru = (old.passthru or { }) // {
          inherit fontsConfig;
          inherit setupFontsConfigScript;
          inherit enableWebdriver;
        };
      });
      "sydtest-webdriver-screenshot" = enableWebdriver (sydtestPkg "sydtest-webdriver-screenshot");
      "sydtest-webdriver-yesod" = enableWebdriver (sydtestPkg "sydtest-webdriver-yesod");
      "sydtest-misbehaved-test-suite" = sydtestPkg "sydtest-misbehaved-test-suite";
    } //
    # https://github.com/mongodb-haskell/mongodb/issues/143
    optionalAttrs (versionOlder mongodb.version "6.0") {
      "sydtest-mongo" = (enableMongo (sydtestPkg "sydtest-mongo")).overrideAttrs (old: {
        passthru = (old.passthru or { }) // {
          inherit enableMongo;
        };
      });
    };

in
{
  inherit sydtestPackages;

  sydtestRelease = symlinkJoin {
    name = "sydtest-release";
    paths = attrValues self.sydtestPackages;
  };
  webdriver =
    if versionAtLeast self.aeson.version "2" && versionOlder super.webdriver.version "0.10"
    then
      (self.callCabal2nix "webdriver"
        (builtins.fetchGit
          {
            url = "https://github.com/codedownio/hs-webdriver";
            rev = "e63f96fc9ebc82b70a7ea9bf3ba0c1dea5c80048";
          })
        { })
    else super.webdriver;
} // sydtestPackages
