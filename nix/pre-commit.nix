{ sources ? import ./sources.nix
}:
let
  nix-pre-commit-hooks = import sources.pre-commit-hooks;
in
{
  tools = with nix-pre-commit-hooks; [
    hlint
    hpack
    nixpkgs-fmt
    ormolu
  ];
  check = nix-pre-commit-hooks.run {
    src = ../.;
    hooks = {
      hlint.enable = true;
      hpack.enable = true;
      nixpkgs-fmt.enable = true;
      ormolu.enable = true;
    };
  };
}
