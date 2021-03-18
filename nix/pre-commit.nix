let
  sources = import ./sources.nix;
  nix-pre-commit-hooks = import sources.pre-commit-hooks;

in
{
  tools = with nix-pre-commit-hooks.tools; [
    hlint
    nixpkgs-fmt
    ormolu
  ];
  check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      nixpkgs-fmt.enable = true;
      hlint.enable = true;
      ormolu.enable = true;
    };
  };
}
