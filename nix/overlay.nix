final: previous:
with final.haskell.lib;

{
  sydtestPackages =
    let
      sydtestPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              failOnAllWarnings (
                final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
              )
            )
            (final.haskellPackages.autoexporter)
        );
      sydtestPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (sydtestPkg name);
      sydtestPkgWithOwnComp = name: sydtestPkgWithComp name name;

    in
    {
      "sydtest" = sydtestPkg "sydtest";
      "sydtest-hspec" = sydtestPkg "sydtest-hspec";
      "sydtest-aeson" = sydtestPkg "sydtest-aeson";
      "sydtest-discover" = sydtestPkgWithOwnComp "sydtest-discover";
      "sydtest-persistent" = sydtestPkg "sydtest-persistent";
      "sydtest-persistent-sqlite" = sydtestPkg "sydtest-persistent-sqlite";
      "sydtest-process" = sydtestPkg "sydtest-process";
      "sydtest-servant" = sydtestPkg "sydtest-servant";
      "sydtest-typed-process" = sydtestPkg "sydtest-typed-process";
      "sydtest-wai" = sydtestPkg "sydtest-wai";
      "sydtest-yesod" = sydtestPkg "sydtest-yesod";
      "sydtest-amqp" = overrideCabal (sydtestPkg "sydtest-amqp") (old: {
        testDepends = (old.testDepends or [ ]) ++ [ final.rabbitmq-server ];
        # Turn off testing because the rabbitmq server doesn't actually work on older nixpkgs versions.
        doCheck = false;
      });
      "sydtest-rabbitmq" = overrideCabal (sydtestPkg "sydtest-rabbitmq") (old: {
        testDepends = (old.testDepends or [ ]) ++ [ final.rabbitmq-server ];
        # Turn off testing because the rabbitmq server doesn't actually work on older nixpkgs versions.
        doCheck = false;
      });
      "sydtest-hedis" = overrideCabal (sydtestPkg "sydtest-hedis") (old: {
        testDepends = (old.testDepends or [ ]) ++ [ final.redis ];
      });
      "sydtest-mongo" = overrideCabal (sydtestPkg "sydtest-mongo") (old: {
        buildDepends = (old.buildDepends or [ ]) ++ [ final.mongodb ];
        preCheck = (old.preCheck or "") + ''
          export NIX_REDIRECTS=/etc/protocols=${final.iana-etc}/etc/protocols
          export LD_PRELOAD=${final.libredirect}/lib/libredirect.so
        '';
        postCheck = (old.postCheck or "") + ''
          unset NIX_REDIRECTS LD_PRELOAD
        '';
      });
    };

  sydtestRelease =
    final.symlinkJoin {
      name = "sydtest-release";
      paths = final.lib.attrValues final.sydtestPackages;
    };

  # Remove after https://github.com/NixOS/nixpkgs/pull/124157 is in the nixpkgs we use.
  libredirect = previous.libredirect.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./fix-libredirect-for-subprocesses.patch ];
  });

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                let
                  # envparse
                  envparseRepo =
                    final.fetchFromGitHub {
                      owner = "supki";
                      repo = "envparse";
                      rev = "de5944fb09e9d941fafa35c0f05446af348e7b4d";
                      sha256 =
                        "sha256:0piljyzplj3bjylnxqfl4zpc3vc88i9fjhsj06bk7xj48dv3jg3b";
                    };
                  envparsePkg =
                    dontCheck (
                      self.callCabal2nix "envparse" envparseRepo { }
                    );

                in
                final.sydtestPackages // {
                  envparse = envparsePkg;
                  # The haskell package mongoDB-2.7.0.0 is marked as broken, these three un-break it.
                  bson = appendConfigureFlag
                    (super.bson.override {
                      network = self.network-bsd;
                    }) "-f-_old_network";
                }
            );
      }
    );
}
