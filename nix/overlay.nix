final: previous:
let
  defCompiler = "ghc${previous.lib.strings.replaceStrings ["."] [""] previous.haskellPackages.ghc.version}";
  gitignoreSrc = final.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore.nix";
    # put the latest commit sha of gitignore Nix library here:
    rev = "5b9e0ff9d3b551234b4f3eb3983744fa354b17f1";
    # use what nix suggests in the mismatch message here:
    sha256 = "01l4phiqgw9xgaxr6jr456qmww6kzghqrnbc7aiiww3h6db5vw53";
  };
  inherit (import gitignoreSrc { inherit (final) lib; }) gitignoreSource;
in with final.haskell.lib;
{
  sydtestPackages =
    compiler:
    let
      sydtestPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              buildStrictly (
                final.haskell.packages.${compiler}.callCabal2nixWithOptions name (gitignoreSource (../. + "/${name}")) "--no-hpack" { }
              )
            )
            (final.haskell.packages.${compiler}.autoexporter)
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
      paths = final.lib.attrValues (final.sydtestPackages defCompiler);
    };

  haskell = previous.haskell // {
    packages = final.lib.mapAttrs
      (compiler: _:
        previous.haskell.packages.${compiler}.override (
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
                  _:
                  _:
                    final.sydtestPackages compiler
                );
          }
        )
      )
      previous.haskell.packages;
  };

  haskellPackages = final.haskell.packages.${defCompiler};

}
