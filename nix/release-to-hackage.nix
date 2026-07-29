{ lib, writeShellApplication, curl, haskell, haskellPackages }:

# Upload this repository's packages to Hackage, skipping the versions that are
# already there.
#
# A package counts as publishable when its cabal file has a synopsis, which
# cabal2nix turns into meta.description.  Hackage rejects a package without
# one, and the in-repo example and test-suite packages deliberately have none,
# so this keeps them out without a second list to maintain.
#
# The script asks Hackage which versions exist and uploads only what is
# missing.  Running it publishes exactly the packages whose version was bumped
# since the last run, and does nothing at all when none were, so it is safe to
# run on every push.
#
# Packages go up in alphabetical order, so a dependency can land after
# something that depends on it.  Hackage accepts an upload whose bounds are
# not yet satisfiable, so that only matters to someone resolving in the window
# between the two uploads.
#
# An upload cannot be undone -- a version on Hackage can only be deprecated --
# which is why it needs HACKAGE_API_KEY to do anything, and why nix-ci.nix
# runs it from master only.  Pass --dry-run to see what it would upload.

let
  publishable = lib.filterAttrs (_: p: p.meta ? description) haskellPackages.sydtestPackages;
  manifestLine = name: p:
    "${name} ${p.version} ${haskell.lib.sdistTarball p}/${name}-${p.version}.tar.gz";
  manifest = lib.concatStringsSep "\n" (lib.mapAttrsToList manifestLine publishable);
in
writeShellApplication {
  name = "release-to-hackage";
  runtimeInputs = [ curl ];
  text = ''
    dry_run=""
    if [[ "''${1:-}" == "--dry-run" ]]; then
      dry_run=yes
    elif [[ "$#" -gt 0 ]]; then
      echo "usage: release-to-hackage [--dry-run]" >&2
      exit 1
    fi

    if [[ -z "''${HACKAGE_API_KEY:-}" && -z "$dry_run" ]]; then
      echo "HACKAGE_API_KEY is not set; refusing to upload." >&2
      exit 1
    fi

    while read -r pname version tarball; do
      # --http1.1 because Hackage resets HTTP/2 streams under a burst of
      # requests, which is exactly what a repository of this many packages
      # sends.
      if ! status="$(curl -sS --http1.1 --retry 3 -o /dev/null -w '%{http_code}' \
        "https://hackage.haskell.org/package/$pname-$version/$pname.cabal")"; then
        echo "$pname-$version: could not ask Hackage whether this version exists" >&2
        exit 1
      fi
      case "$status" in
        200)
          echo "$pname-$version is already on Hackage"
          ;;
        404)
          if [[ -n "$dry_run" ]]; then
            echo "$pname-$version would be uploaded from $tarball"
          else
            echo "$pname-$version: uploading"
            curl -sS --http1.1 --fail-with-body \
              -H "Authorization: X-ApiKey $HACKAGE_API_KEY" \
              -F "package=@$tarball" \
              https://hackage.haskell.org/packages/
            echo
          fi
          ;;
        *)
          echo "$pname-$version: Hackage answered $status, uploading nothing" >&2
          exit 1
          ;;
      esac
    done <<'MANIFEST'
    ${manifest}
    MANIFEST
  '';
}
