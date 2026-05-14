{ stdenv, jq }:

# Fail if any mutation in the report survived, and optionally if any were uncovered.
# Takes a report derivation produced by compileMutationReport and exits
# non-zero when the report shows surviving (or uncovered) mutations.

{ name ? "assert-mutation-score" # name for the derivation
, report # a derivation produced by compileMutationReport; must contain report.json
, assertNoneUncovered ? true # if true, also fail when any mutations are uncovered
}:

stdenv.mkDerivation {
  inherit name;
  nativeBuildInputs = [ jq ];
  # srcs = [] suppresses stdenv's default unpack phase without disabling the
  # rest of the generic build, so buildCommand runs in a clean empty sandbox.
  srcs = [ ];
  passthru = { inherit report; };
  buildCommand = ''
    survived=$(jq '.survived' ${report}/report.json)
    killed=$(jq '.killed' ${report}/report.json)
    uncovered=$(jq '.uncovered' ${report}/report.json)
    total=$(( killed + survived + uncovered ))

    echo "Results: $killed killed, $survived survived, $uncovered uncovered out of $total total"

    fail=0

    if [ "$survived" != "0" ]; then
      echo ""
      echo "FAIL: $survived mutation(s) survived — not all mutations were killed."
      echo "Add or strengthen tests to kill the surviving mutations."
      fail=1
    fi

    ${if assertNoneUncovered then ''
    if [ "$uncovered" != "0" ]; then
      echo ""
      echo "FAIL: $uncovered mutation(s) uncovered — not all mutations are reached by any test."
      echo "Add tests to cover the uncovered mutations."
      fail=1
    fi
    '' else ""}

    echo ""
    cat ${report}/report.txt

    echo "Full report:               ${report}/report.txt"
    echo "Machine-readable report:   ${report}/report.json"
    echo ""

    if [ "$fail" != "0" ]; then
      echo "FAIL: $survived Surviving, $uncovered uncovered mutations."

      exit 1
    else
      echo "PASS: All $total mutation(s) accounted for."

      mkdir -p $out
      cp ${report}/report.txt $out/report.txt
      cp ${report}/report.json $out/report.json
    fi
  '';
}
