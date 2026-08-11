# NixCI configuration.
#
# NixCI evaluates this file when it exists and only falls back to the flake's
# 'nix-ci' output when it does not, so this file is the whole configuration:
# anything left out here is not configured at all.  That is why the cachix
# settings live here rather than in flake.nix.
#
# It also has to be self-contained.  NixCI reads it straight out of the forge
# without cloning, so an import of another file is rejected rather than read.
let
  system = "x86_64-linux";
in
{
  cachix = {
    name = "sydtest";
    public-key = "sydtest.cachix.org-1:fyby3c42t+0iTABcLd/R3POxzJhCQ/9gYM7Sh879+9w=";
  };
  # Publish to Hackage from master only.  The script uploads exactly those
  # packages whose version is not on Hackage yet, so a push that bumps no
  # version releases nothing.  The script comes from the release-to-hackage
  # flake input.
  #
  # HACKAGE_API_KEY is set in the repository's Secrets overview on NixCI.
  deploy = {
    release-to-hackage = {
      package = "packages.${system}.release-to-hackage";
      branches = [ "master" ];
      secrets = [ "HACKAGE_API_KEY" ];
    };
  };
}
