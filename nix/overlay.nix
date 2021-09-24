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
      # The haskell package tmp-postgres-1.34.1.0 is marked as broken.
      # "sydtest-persistent-postgresql" = overrideCabal (sydtestPkg "sydtest-persistent-postgresql") (old: {
      #   testDepends = (old.testDepends or [ ]) ++ [ final.postgresql ];
      # });
      "sydtest-mongo" = overrideCabal (sydtestPkg "sydtest-mongo") (old: {
        buildDepends = (old.buildDepends or [ ]) ++ [ final.mongodb ];
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

  # Remove after https://github.com/NixOS/nixpkgs/pull/124157 is in the nixpkgs
  # we use.
  # The libredirect program does not work with subprocesses, like the test
  # suite when cabal test execve's it.
  # So we need to apply a fix that is already in nixpkgs master but not yet in
  # the nixpkgs version that we use.
  # We put it in the ./fix-libredirect-for-subprocesses.patch file in this
  # directory and apply it to our current libredirect here.
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
                  mongoDB = unmarkBroken super.mongoDB;
                  bson = appendConfigureFlag
                    ((unmarkBroken super.bson).override {
                      network = self.network-bsd;
                    }) "-f-_old_network";
                }
            );
      }
    );
}
