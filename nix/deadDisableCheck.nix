{ haskellPackages, runCommand, lib }:

# Compile check: the mutation plugin must reject a disable annotation that
# disables nothing, and must accept one that disables something.
#
# Why this needs compiling at all. The decision behind each of these errors is
# unit-tested in the plugin's own test suite ('deadDisables',
# 'deadModuleDisables'), but those tests hand the decision a ready-made set of
# operators that fired. Producing that set from real syntax is the measuring
# walk, which is the whole mechanism, and nothing else exercises it: the
# example package cannot carry a dead annotation, because a dead annotation
# fails to compile there too. So these modules live here, outside any
# component, and a compiler is the only thing that can judge them.
#
# Why 'ghc -c' and not the GHC API from a test. Running GHC in-process would
# need the project's package databases (the plugin injects an import of its
# runtime module), which are discoverable only through environment that
# differs between stack, cabal and nix -- and it would add a second GHC-API
# surface to keep working across GHC upgrades, on top of the plugin's own.
# 'ghc -c' is stable across both.
#
# Each module in test_resources/dead-disable is its own derivation, so they
# compile in parallel and a failure names the module. A module with a
# companion '.expected' file must fail to compile with that text in the
# output; a module without one must compile. Both directions matter: the
# second is what stops the check from passing by rejecting everything.

let
  ghc = haskellPackages.ghcWithPackages (p: [ p.sydtest-mutation-plugin ]);

  resources = ../sydtest-mutation-plugin/test_resources/dead-disable;

  pluginFlags = lib.concatStringsSep " " [
    "-fplugin=Test.Syd.Mutation.Plugin"
    # Where to find the plugin module itself, used at compile time.
    "-plugin-package=sydtest-mutation-plugin"
    # Makes the runtime module the plugin injects an import of resolvable.
    "-package=sydtest-mutation-plugin"
  ];

  moduleNames = lib.pipe (builtins.readDir resources) [
    builtins.attrNames
    (builtins.filter (lib.hasSuffix ".hs"))
    (map (lib.removeSuffix ".hs"))
  ];

  # The text a module's rejection must contain, or null when the module is
  # expected to compile.
  expectationOf = name:
    let expected = resources + "/${name}.expected";
    in if builtins.pathExists expected then expected else null;

  # An expectation whose module was renamed or removed would otherwise sit
  # there unused, and its module would silently become one that merely has to
  # compile -- so the check would go on passing while no longer checking
  # anything.  Refuse to build instead.
  strayExpectations = lib.pipe (builtins.readDir resources) [
    builtins.attrNames
    (builtins.filter (lib.hasSuffix ".expected"))
    (map (lib.removeSuffix ".expected"))
    (builtins.filter (name: !(builtins.elem name moduleNames)))
  ];

  compileCheck = name:
    let
      expected = expectationOf name;
      verdict =
        if expected == null
        then ''
          if [ "$compiled" = no ]; then
            echo "FAIL: ${name} must compile: every annotation in it disables something."
            exit 1
          fi
          echo "PASS: ${name} compiled, as it should"
        ''
        else ''
          if [ "$compiled" = yes ]; then
            echo "FAIL: ${name} compiled, but its annotation disables nothing and must be rejected."
            exit 1
          fi
          # A pattern file whose only line is blank matches any rejection at
          # all, which would accept a module rejected for some unrelated
          # reason, so demand that the expectation say something.
          if ! grep -q '[^[:space:]]' ${expected}; then
            echo "FAIL: ${name}.expected is blank, so it would match any rejection."
            exit 1
          fi
          if ! grep -qF -f ${expected} compile.log; then
            echo "FAIL: ${name} was rejected, but not for the reason ${name}.expected gives:"
            cat ${expected}
            exit 1
          fi
          echo "PASS: ${name} was rejected, as it should be"
        '';
    in
    runCommand "dead-disable-${name}"
      {
        nativeBuildInputs = [ ghc ];
      } ''
      cp ${resources}/${name}.hs .
      if ghc -c -O0 ${pluginFlags} ${name}.hs > compile.log 2>&1
      then compiled=yes
      else compiled=no
      fi
      cat compile.log
      ${verdict}
      cp compile.log $out
    '';

in
assert lib.assertMsg (strayExpectations == [ ])
  "dead-disable expectations with no module of their own: ${lib.concatStringsSep ", " strayExpectations}";
runCommand "mutation-dead-disable" { } ''
  # Naming each module's derivation is what depends on it, and so what makes
  # this fail when any of them does.
  ${lib.concatMapStringsSep "\n" (check: "echo ${check}") (map compileCheck moduleNames)}
  echo "PASS: the plugin rejects every dead disable annotation and accepts every live one"
  touch $out
''
