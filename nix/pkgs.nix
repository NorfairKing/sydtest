{ sources ? import ./sources.nix
, pkgsf ? import sources.nixpkgs
, system ? builtins.currentSystem
}:
let
  pkgs = pkgsf { inherit system; };
  sydtestPkgs =
    pkgsf {
      inherit system;
      overlays =
        [
          (import (sources.validity + "/nix/overlay.nix"))
          (import (sources.autodocodec + "/nix/overlay.nix"))
          (import (sources.safe-coloured-text + "/nix/overlay.nix"))
          (final: previous: { niv = (import sources.niv { }).niv; })
          (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
          (import ./overlay.nix)
          (import ./fixes-overlay.nix)
        ];
      config.allowUnfree = true;
    };
in
sydtestPkgs
