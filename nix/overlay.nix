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
              buildStrictly (
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
        # Turn off testing because it hangs for unknown reasons?
        doCheck = false;
      });
      "sydtest-rabbitmq" = overrideCabal (sydtestPkg "sydtest-rabbitmq") (old: {
        testDepends = (old.testDepends or [ ]) ++ [ final.rabbitmq-server ];
        # Turn off testing because it hangs for unknown reasons?
        doCheck = false;
      });
      "sydtest-hedis" = overrideCabal (sydtestPkg "sydtest-hedis") (old: {
        testDepends = (old.testDepends or [ ]) ++ [ final.redis ];
      });
      "sydtest-persistent-postgresql" = overrideCabal (sydtestPkg "sydtest-persistent-postgresql") (old: {
        testDepends = (old.testDepends or [ ]) ++ [ final.postgresql ];
      });
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
                final.sydtestPackages
            );
      }
    );
}
