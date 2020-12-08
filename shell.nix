(import ./nix/pkgs.nix).mkShell {
  inherit ((import ./ci.nix).pre-commit-check) shellHook;
}
