let
  pkgs = import ./nix/pkgs.nix { };
  pre-commit-check = (import ./ci.nix { inherit pkgs; }).pre-commit-check;
in
pkgs.haskell.lib.buildStackProject {
  name = "sydtest";
  buildInputs = with pkgs; [
    coreutils
    rabbitmq-server
    zlib
    redis
  ];
  shellHook = ''
    ${pre-commit-check.shellHook}
  '';
}
