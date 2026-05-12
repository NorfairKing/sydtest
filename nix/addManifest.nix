{ haskell, mutationPlugin }:

# Wrap a Haskell package so that the mutation plugin runs during compilation
# and writes the mutation manifest to a separate 'manifest' output.
#
# The plugin flag and sydtest-mutation-plugin dependency are injected automatically via
# GHC flags and Nix buildDepends, so the wrapped package does not need to
# declare them in its cabal file.
#
# The manifest output path is communicated to the plugin via the
# MUTATION_MANIFEST_DIR environment variable (set in preBuild), because Nix output
# store paths are not available as Nix strings at evaluation time and cannot be
# interpolated into configureFlags or buildFlags.

{ exceptions ? [ ] # list of module names to skip during instrumentation
, disabledMutations ? [ ] # list of mutation type names to disable globally (e.g. [ "Arith" "BoolLit" ])
, debug ? false # print each mutation site as it is recorded (for debugging the plugin)
, ghcMemLimit ? "16g" # RTS heap limit for GHC during instrumented compilation (e.g. "8g", "16g")
}:
pkg: # the Haskell package derivation to wrap

let
  pluginOpts = builtins.map (e: "--exception=" + e) exceptions
    ++ builtins.map (m: "--disable-mutation=" + m) disabledMutations
    ++ (if debug then [ "--debug" ] else [ ]);
  stringOpt = arg: "--ghc-options=-fplugin-opt=Test.Syd.Mutation.Plugin:${arg}";
  exceptionConfigureFlags = builtins.map stringOpt pluginOpts;
in
(haskell.lib.overrideCabal pkg (old: {
  # Disable haddock: the haddock pass runs GHC with the plugin but without
  # the per-component plugin options, causing exception modules to be
  # instrumented despite being listed in 'exceptions'.
  doHaddock = false;
  # Skip optimization: the instrumented code only needs to run correctly,
  # not efficiently. -O causes GHC to spend superlinear time/memory
  # simplifying the nested ifMutation case expressions.
  enableStaticLibraries = false;
  buildDepends = (old.buildDepends or [ ]) ++ [ mutationPlugin ];
  buildFlags = (old.buildFlags or [ ]) ++ [
    "--ghc-option=-fplugin=Test.Syd.Mutation.Plugin"
    "--ghc-option=-plugin-package=sydtest-mutation-plugin"
    # The parsedResultAction injects 'import Test.Syd.Mutation.Plugin.Runtime ()'; expose
    # the package so GHC can resolve that module in the compiled modules.
    "--ghc-option=-package=sydtest-mutation-plugin"
  ];
  configureFlags = (old.configureFlags or [ ]) ++ exceptionConfigureFlags
    # Disable optimization so GHC doesn't spend superlinear time/memory
    # simplifying the nested ifMutation case expressions the plugin generates.
    ++ [ "--disable-optimization" ]
    # Override the default -j16 -A64M that Nix injects: use single-threaded
    # compilation with a small allocation area, and cap the heap.
    ++ [
    "--ghc-option=-j1"
    "--ghc-option=+RTS"
    "--ghc-option=-A32M"
    "--ghc-option=-M${ghcMemLimit}"
    "--ghc-option=-RTS"
  ];
  preBuild = (old.preBuild or "") + ''
    echo "mutation-nix: setting MUTATION_MANIFEST_DIR=$manifest"
    mkdir -p "$manifest"
    export MUTATION_MANIFEST_DIR="$manifest"
  '';
  postBuild = (old.postBuild or "") + ''
    echo "mutation-nix: manifest output at $manifest:"
    ls -la "$manifest/" || echo "(empty)"
  '';
})).overrideAttrs (old: {
  outputs = (old.outputs or [ "out" ]) ++ [ "manifest" ];
})
