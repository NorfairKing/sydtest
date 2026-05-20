{ haskellPackages, runCommand, jq, diffutils }:

# Golden check: assert that the mutation plugin produces the manifest we
# expect when run against sydtest-mutation-example.
#
# Builds the example library through the plugin, pretty-prints each manifest
# JSON file, and diffs the result against the committed tree in
# sydtest-mutation-example/test_resources/manifests.
#
# A diff means an operator's output has changed: either intentionally (run
# 'mutation-manifest-update' below to refresh the goldens) or as a regression
# (one of the operators no longer fires, fires at the wrong span, picks the
# wrong replacement, etc.).
#
# The 'passthru.actual' attribute exposes the pretty-printed manifest tree so
# 'nix build .#checks.x86_64-linux.mutation-manifest-example.actual' produces
# a fresh tree that can be copied over the goldens.

let
  instrumented = haskellPackages.sydtest.addManifest { configFile = ./mutation.yaml; } haskellPackages.sydtest-mutation-example;

  expected = ../sydtest-mutation-example/test_resources/manifests;

  # Pretty-print the plugin's compact manifest output for diff-friendly goldens.
  actual = runCommand "sydtest-mutation-example-manifest-pretty"
    {
      nativeBuildInputs = [ jq ];
    } ''
    mkdir -p $out
    for f in ${instrumented.manifest}/*.json; do
      name=$(basename "$f")
      jq --sort-keys . "$f" > "$out/$name"
    done
  '';

in
runCommand "mutation-manifest-example"
{
  nativeBuildInputs = [ diffutils ];
  passthru = { inherit actual; };
} ''
  if diff -ruN ${expected} ${actual}; then
    echo "PASS: mutation manifest matches the committed goldens"
    ln -s ${actual} $out
  else
    echo ""
    echo "FAIL: mutation manifest differs from the committed goldens."
    echo ""
    echo "If this change is intentional (e.g. you added or modified a mutation operator),"
    echo "update the goldens with:"
    echo ""
    echo "  nix build .#checks.x86_64-linux.mutation-manifest-example.actual"
    echo "  cp -r result/. sydtest-mutation-example/test_resources/manifests/"
    echo "  chmod -R u+w sydtest-mutation-example/test_resources/manifests"
    echo ""
    exit 1
  fi
''
