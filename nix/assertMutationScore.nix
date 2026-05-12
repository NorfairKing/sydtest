{ stdenv }:

# Fail if any mutation in the report survived.
# Takes a report derivation produced by compileMutationReport and exits
# non-zero when the report shows surviving mutations.

{ name ? "assert-mutation-score" # name for the derivation
, report # a derivation produced by compileMutationReport; must contain report.txt
}:

stdenv.mkDerivation {
  inherit name;
  srcs = [ ];
  buildCommand = ''
    echo "mutation-nix: checking mutation report at ${report}/report.txt"
    cat ${report}/report.txt

    survived=$(grep "^Survived:" ${report}/report.txt | awk '{print $2}' || echo "0")
    killed=$(grep "^Killed:" ${report}/report.txt | awk '{print $2}' || echo "0")
    total=$(( killed + survived ))

    echo ""
    echo "Results: $killed killed, $survived survived out of $total total"

    mkdir -p $out
    cp ${report}/report.txt $out/report.txt
    if [ -f ${report}/report.json ]; then
      cp ${report}/report.json $out/report.json
    fi

    if [ "$survived" != "0" ]; then
      echo ""
      echo "FAIL: $survived mutation(s) survived — not all mutations were killed."
      echo "Add or strengthen tests to kill the surviving mutations."
      echo "Full report: $out/report.txt"
      if [ -f $out/report.json ]; then
        echo "Machine-readable report: $out/report.json"
      fi
      exit 1
    fi

    echo "PASS: All $total mutation(s) were killed."
  '';
}
