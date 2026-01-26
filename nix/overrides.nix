{ lib
, haskell
, libredirect
, redis
, postgresql
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
        })
    );

  fontsConfig = callPackage ./fonts-conf.nix { };
  setupFontsConfigScript = ''
    export FONTCONFIG_SYSROOT=${fontsConfig}
  '';
  enableWebdriver = haskellPkg: overrideCabal haskellPkg (old:
    let
      webdriverDeps = [
        chromium
        chromedriver
        selenium-server-standalone
      ];
    in
    {
      testDepends = (old.testDepends or [ ]) ++ webdriverDeps;
      preConfigure = (old.preConfigure or "") + setupFontsConfigScript;
      passthru = (old.passthru or { }) // { inherit webdriverDeps; };
    });

  sydtestPackages = {
    "sydtest" = sydtestPkg "sydtest";
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
    "sydtest-test" = overrideCabal (sydtestPkg "sydtest-test") (old: {
      # We turn off tests on other versions because they generate different
      # random data and/or add different data to the callstack.
      # This makes the output tests fail.
      doCheck = self.ghc.version == "9.10.3";
    });
    "sydtest-misbehaved-test-suite" = sydtestPkg "sydtest-misbehaved-test-suite";
  };

in
{
  inherit sydtestPackages;

  sydtestRelease = symlinkJoin {
    name = "sydtest-release";
    paths = attrValues self.sydtestPackages;
    passthru = self.sydtestPackages;
  };
  # Until https://github.com/jfischoff/tmp-postgres/issues/281
  tmp-postgres =
    dontCheck (self.callCabal2nix "tmp-postgres"
      (builtins.fetchGit
        {
          url = "https://github.com/jfischoff/tmp-postgres";
          rev = "7f2467a6d6d5f6db7eed59919a6773fe006cf22b";
        })
      { });
} // sydtestPackages
