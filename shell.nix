let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { };
  pre-commit = import ./nix/pre-commit.nix;
in
pkgs.haskell.lib.buildStackProject {
  name = "sydtest-shell";
  buildInputs = with pkgs; [
    coreutils
    zlib
    (import sources.niv { inherit pkgs; }).niv
    rabbitmq-server
    redis
  ];
  shellHook = ''
    ${pre-commit.check.shellHook}
  '';
}
