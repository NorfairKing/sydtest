{ haskell, mutationPlugin }:

# Wrap a Haskell package so that the mutation plugin runs during compilation
# and writes the mutation manifest to a separate 'manifest' output.
#
# The manifest lists every mutation site found in the package's source, one
# per line, as tab-separated fields:
#
#   <module> <TAB> <operator> <TAB> <startLine> <TAB> <startCol> <TAB> <endCol>
#
# The plugin flag and sydtest-mutation-plugin dependency are injected automatically via
# GHC flags and Nix buildDepends, so the wrapped package does not need to
# declare them in its cabal file.
#
# The manifest output path is communicated to the plugin via the
# MUTATION_MANIFEST environment variable (set in preBuild), because Nix output
# store paths are not available as Nix strings at evaluation time and cannot be
# interpolated into configureFlags or buildFlags.
#
# NOTE: Currently this produces one manifest per package. In the future we
# will want to combine manifests from multiple packages before passing them to
# runMutations, so that a single runner invocation exercises mutations across
# the whole codebase. The natural place to do that combination is in
# makeMutationReport, analogously to how dekking's compileCoverageReport
# merges coverables/coverage from multiple packages.

{ exceptions ? [ ] # list of module names to skip during instrumentation
}:
pkg: # the Haskell package derivation to wrap

let
  pluginOpts = builtins.map (e: "--exception=" + e) exceptions;
  stringOpt = arg: "--ghc-options=-fplugin-opt=Test.Syd.Mutation.Plugin:${arg}";
  exceptionConfigureFlags = builtins.map stringOpt pluginOpts;
in
(haskell.lib.overrideCabal pkg (old: {
  # Disable haddock: the haddock pass runs GHC with the plugin but without
  # the per-component plugin options, causing exception modules to be
  # instrumented despite being listed in 'exceptions'.
  doHaddock = false;
  # Prevents ld.gold "requires unsupported dynamic reloc 11" errors that arise
  # when -package=sydtest-mutation-plugin is passed to GHC. See dekking's addCoverables.nix.
  enableStaticLibraries = false;
  buildDepends = (old.buildDepends or [ ]) ++ [ mutationPlugin ];
  buildFlags = (old.buildFlags or [ ]) ++ [
    "--ghc-option=-fplugin=Test.Syd.Mutation.Plugin"
    "--ghc-option=-plugin-package=sydtest-mutation-plugin"
    # The parsedResultAction injects 'import Test.Syd.Mutation.Plugin.Runtime ()'; expose
    # the package so GHC can resolve that module in the compiled modules.
    "--ghc-option=-package=sydtest-mutation-plugin"
  ];
  configureFlags = (old.configureFlags or [ ]) ++ exceptionConfigureFlags;
  preBuild = (old.preBuild or "") + ''
    echo "mutation-nix: setting MUTATION_MANIFEST=$manifest/mutation.manifest"
    mkdir -p "$manifest"
    export MUTATION_MANIFEST="$manifest/mutation.manifest"
  '';
  postBuild = (old.postBuild or "") + ''
    echo "mutation-nix: manifest output at $manifest:"
    ls -la "$manifest/" || echo "(empty)"
  '';
})).overrideAttrs (old: {
  outputs = (old.outputs or [ "out" ]) ++ [ "manifest" ];
})
