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
      "sydtest-discover" = sydtestPkgWithOwnComp "sydtest-discover";
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
                }
            );
      }
    );
}
