{ lib }:

# Read top-level @executable@ and @test-suite@ stanza names out of a Cabal
# file at Nix evaluation time.
#
# A "source root" is a directory containing one package subdirectory per
# package, where each subdirectory is named after the package and contains
# a @<pname>.cabal@ file.  Most flake inputs in this repo (opt-env-conf,
# safe-coloured-text, ...) follow that layout.

let
  # Match a top-level stanza line of the form @<keyword> <name>...@ (the
  # keyword is matched case-insensitively).  Returns the captured name or
  # 'null'.
  matchStanza = keyword: l:
    let
      perCharBracket = c: "[${lib.toLower c}${lib.toUpper c}]";
      charBrackets =
        lib.concatStrings (map perCharBracket (lib.stringToCharacters keyword));
      pattern = "${charBrackets}[[:space:]]+([^[:space:]]+).*";
    in
    builtins.match pattern l;

  # Extract every name that follows the given stanza keyword.
  namesFor = keyword: cabalText:
    let
      lines = lib.splitString "\n" cabalText;
      hits = builtins.filter (m: m != null) (map (matchStanza keyword) lines);
    in
    map (m: builtins.head m) hits;

  # Components of one cabal file at @path@.
  componentsOf = path:
    let text = builtins.readFile path;
    in
    {
      executables = namesFor "executable" text;
      testSuites = namesFor "test-suite" text;
    };

  # Look up @pname@ under one of @roots@ and return its components.  The
  # expected layout is @<root>/<pname>/<pname>.cabal@.  The first root that
  # contains a cabal file for the package wins; if no root does, throw
  # with a useful error rather than silently producing empty lists.
  forPackage = roots: pname:
    let
      candidates = map (root: "${toString root}/${pname}/${pname}.cabal") roots;
      existing = builtins.filter builtins.pathExists candidates;
    in
    if existing == [ ]
    then throw "cabalComponents: no cabal file for '${pname}' under any of: ${lib.concatStringsSep ", " (map toString roots)}"
    else componentsOf (builtins.head existing);

  # Build a map @pname -> { executables, testSuites }@ for every package in
  # @pnames@ by searching @roots@.
  forPackages = roots: pnames:
    lib.genAttrs pnames (forPackage roots);
in
{
  inherit forPackage forPackages componentsOf namesFor;
}
