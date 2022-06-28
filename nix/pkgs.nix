{ sources ? import ./sources.nix
, nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
}:
import nixpkgs {
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
}
