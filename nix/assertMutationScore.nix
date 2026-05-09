{ stdenv, mutationDriver }:

# Fail if any mutation in the report survived, and optionally if any were
# uncovered.  Takes a report derivation produced by mutationCheck (must
# contain report.json and report.txt) and exits non-zero when the report
# shows surviving (or uncovered) mutations.  On success, populates $out
# with symlinks to the report files.
#
# All of the work — assertion check, rendering, symlinking — lives in
# 'sydtest-mutation-driver assert-score'.  This file is just the
# mkDerivation wrapping.

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
      --out-dir "$out" \
      ${report}
  '';
}
