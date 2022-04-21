let
  pkgs = import ./nix/pkgs.nix { };
  pre-commit = import ./nix/pre-commit.nix;
in
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
