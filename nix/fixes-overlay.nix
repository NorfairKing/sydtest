final: previous:
with final.haskell.lib;
let
  sources = import ./sources.nix;
in

{
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
                {
                  webdriver = final.haskellPackages.callCabal2nix "webdriver" sources.webdriver { };
                }
            );
      }
    );
}
