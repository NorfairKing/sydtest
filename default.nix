let
  sources = import ./nix/sources.nix;
  haskellNix = import sources."haskell.nix" { };
  pkgsf = import sources.nixpkgs;
  pkgs = pkgsf haskellNix.nixpkgsArgs;
in
pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "sydtest-project";
    src = ./.;
  };
}
