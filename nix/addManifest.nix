{ haskell, mutationDriver, mutationPlugin }:

# Wrap a Haskell package so that the mutation plugin runs during compilation
# and writes the mutation manifest to a separate 'manifest' output.
#
# This function is curried: first call it with options, then with the package.
# Example:
#   addManifest { config = { debug = true; }; } haskellPackages.mylib
#
# The plugin flag and sydtest-mutation-plugin dependency are injected
# automatically via GHC flags and Nix buildDepends, so the wrapped package does
# not need to declare them in its cabal file.
#
# The manifest output path is communicated to the plugin via the
# MUTATION_PLUGIN_MANIFEST_DIR environment variable (set in preBuild), because Nix
# output store paths are not available as Nix strings at evaluation time and
# cannot be interpolated into configureFlags or buildFlags.
#
# Plugin-instrumentation tunables (exceptions, disabled mutation types,
# skip-th-splices, debug, ignore) live in the YAML file pointed to by
# 'configFile'. When 'configFile' is null, the plugin uses its built-in
# defaults (instrument everything, no debug output).
#
# Schema for the YAML file: Test.Syd.Mutation.Plugin.OptParse.MutationPluginConfig.

{ configFile ? null # Optional path to a YAML config file; passed to the plugin via --config-file=PATH
, ghcMemLimit ? "16g" # RTS heap limit for GHC during instrumented compilation (e.g. "8g", "16g")
}:
pkg: # the Haskell package derivation to wrap

let
  configConfigureFlags =
    if configFile == null
    then [ ]
    else [ "--ghc-options=-fplugin-opt=Test.Syd.Mutation.Plugin:--config-file=${configFile}" ];
in
(haskell.lib.overrideCabal pkg (old: {
  # Disable haddock: the haddock pass runs GHC with the plugin but without
  # the per-component plugin options, causing exception modules to be
  # instrumented despite being listed in 'exceptions'.
  doHaddock = false;
  # Disable static libraries: building .a archives is unnecessary for
  # mutation testing and slows the build without benefit.
  enableStaticLibraries = false;
  buildDepends = (old.buildDepends or [ ]) ++ [ mutationPlugin ];
  # Limit the main 'Setup build' to the library component. Without this,
  # Cabal would compile every enabled component (test-suites, executables,
  # benchmarks) with the same --ghc-options=-fplugin=... we set below,
  # instrumenting them too. The remaining components are built in
  # postBuild with the plugin loaded but its instrumentation suppressed
  # via the MUTATION_PLUGIN_SKIP env var (see the postBuild comment).
  buildTarget = "lib:${old.pname}";
  buildFlags = (old.buildFlags or [ ]) ++ [
    # Activate the plugin for every compiled module.
    "--ghc-option=-fplugin=Test.Syd.Mutation.Plugin"
    # Tell GHC where to find the plugin itself (the package that defines
    # Test.Syd.Mutation.Plugin).
    "--ghc-option=-plugin-package=sydtest-mutation-plugin"
    # The parsedResultAction injects 'import Test.Syd.Mutation.Plugin.Runtime ()';
    # expose the package so GHC can resolve that module in compiled modules.
    # Distinct from -plugin-package: -plugin-package is for the plugin module
    # used at compile time; -package makes the runtime module visible at build time.
    "--ghc-option=-package=sydtest-mutation-plugin"
  ];
  configureFlags = (old.configureFlags or [ ]) ++ configConfigureFlags
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
    echo "mutation-nix: setting MUTATION_PLUGIN_MANIFEST_DIR=$manifest"
    mkdir -p "$manifest"
    export MUTATION_PLUGIN_MANIFEST_DIR="$manifest"
  '';
  # The main 'Setup build' invocation (driven by buildFlags above) has
  # 'lib:${old.pname}' as its buildTarget so only the library is
  # compiled with the plugin.  We always run a second 'Setup build' (no
  # target) so Cabal compiles every other configured component
  # (executables, test-suites if doCheck=true, benchmarks if
  # doBenchmark=true) with the plugin still loaded but silenced via
  # MUTATION_PLUGIN_SKIP=1.  This way the caller controls which
  # components exist via configure-time flags (`--enable-tests`,
  # `--enable-benchmarks`); addManifest doesn't second-guess it.
  #
  # We cannot drop the plugin flags from the second build because Cabal
  # would see the package set change ('[sydtest-mutation-plugin
  # removed]') and rebuild the library un-instrumented; the runtime
  # kill switch keeps the library's compiled artefacts intact.
  postBuild = (old.postBuild or "") + ''
    echo "mutation-nix: manifest output at $manifest:"
    ls -la "$manifest/" || echo "(empty)"
    echo "mutation-nix: building remaining configured components with plugin silenced"
    MUTATION_PLUGIN_SKIP=1 ./Setup build \
      --ghc-option=-fplugin=Test.Syd.Mutation.Plugin \
      --ghc-option=-plugin-package=sydtest-mutation-plugin \
      --ghc-option=-package=sydtest-mutation-plugin
  '';
  # The library is installed by nixpkgs's default installPhase (which runs
  # 'Setup copy lib:${pname}').  Executables built in postBuild are not
  # copied by that command, so we install them manually here.  Downstream
  # wrappers such as opt-env-conf's installManpagesAndCompletions expect
  # $out/bin/<exe> to exist when their postInstall hooks run, so this must
  # run BEFORE any existing postInstall hooks the caller has already
  # attached.
  #
  # Executable component names are discovered at build time by running
  # the driver's 'list-components' subcommand (which uses the 'Cabal'
  # library) against the package's cabal file, so the caller does not
  # have to enumerate them at Nix evaluation time.
  postInstall = ''
    ${mutationDriver}/bin/sydtest-mutation-driver install-components \
      executables "${old.pname}" "$out/bin"
  '' + (old.postInstall or "");
})).overrideAttrs (old: {
  outputs = (old.outputs or [ "out" ]) ++ [ "manifest" ];
})
