{ stdenv, jq }:

# Fail if any mutation in the report survived.
# Takes a report derivation produced by compileMutationReport and exits
# non-zero when the report shows surviving mutations.

{ name ? "assert-mutation-score" # name for the derivation
, report # a derivation produced by compileMutationReport; must contain report.json
}:

stdenv.mkDerivation {
  inherit name;
  nativeBuildInputs = [ jq ];
  # srcs = [] suppresses stdenv's default unpack phase without disabling the
  # rest of the generic build, so buildCommand runs in a clean empty sandbox.
  srcs = [ ];
  buildCommand = ''
    survived=$(jq '.survived' ${report}/report.json)
    killed=$(jq '.killed' ${report}/report.json)
    total=$(( killed + survived ))

    echo "Results: $killed killed, $survived survived out of $total total"

    if [ "$survived" != "0" ]; then
      echo ""
      echo "FAIL: $survived mutation(s) survived — not all mutations were killed."
      echo "Add or strengthen tests to kill the surviving mutations."
      echo ""
      echo "Full report:"
      cat ${report}/report.txt
      exit 1
    else
      echo "PASS: All $total mutation(s) were killed."
      mkdir -p $out
      cp ${report}/report.txt $out/report.txt
      cp ${report}/report.json $out/report.json
    fi
  '';
}
