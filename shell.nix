{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, pkgs ? import ./nix/pkgs.nix { inherit nixpkgs sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "sydtest-shell";
  buildInputs = with pkgs; [
    chromedriver
    chromium
    coreutils
    mongodb
    niv
    postgresql
    rabbitmq-server
    redis
    selenium-server-standalone
    zlib
  ] ++ pre-commit.tools;
  shellHook = ''
    export TMPDIR=/tmp
    ${pre-commit.check.shellHook}
    ${pkgs.haskellPackages.sydtest-webdriver.setupFontsConfigScript}
  '';
}
