{ stdenv, mutationDriver }:

# Fail if any mutation in the report survived, and optionally if any were
# uncovered.  Takes a report derivation produced by mutationCheck (must
# contain report.json and report.txt) and exits non-zero when the report
# shows surviving (or uncovered) mutations.
#
# All decision logic and rendering live in
# 'sydtest-mutation-driver assert-score'.  This file is just the
# Nix-level boilerplate: invoke the subcommand, and on success symlink
# the report files into $out.

{ name ? "assert-mutation-score" # name for the derivation
, report # a derivation produced by mutationCheck; must contain report.json and report.txt
, assertNoneUncovered ? true # if true, also fail when any mutations are uncovered
}:

stdenv.mkDerivation {
  inherit name;
  # srcs = [] suppresses stdenv's default unpack phase without disabling the
  # rest of the generic build, so buildCommand runs in a clean empty sandbox.
  srcs = [ ];
  passthru = { inherit report; };
  buildCommand = ''
    ${mutationDriver}/bin/sydtest-mutation-driver assert-score \
      ${if assertNoneUncovered then "--assert-none-uncovered" else "--no-assert-none-uncovered"} \
      ${report}

    # The subcommand exits non-zero on a failed assertion, which aborts
    # this build before we reach the symlink step below.  On success we
    # link the two report files into $out so downstream consumers can
    # reach them via this derivation.
    mkdir -p $out
    ln -s ${report}/report.txt $out/report.txt
    ln -s ${report}/report.json $out/report.json
  '';
}
