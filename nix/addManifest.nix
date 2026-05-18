{ haskell, lib, mutationPlugin, writeText }:

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
# MUTATION_MANIFEST_DIR environment variable (set in preBuild), because Nix
# output store paths are not available as Nix strings at evaluation time and
# cannot be interpolated into configureFlags or buildFlags.
#
# All plugin-instrumentation tunables (exceptions, disabled mutation types,
# skip-th-splices, debug) live in the YAML config file rendered from the
# 'config' attrset. When 'config' is {}, the plugin uses its built-in
# defaults (instrument everything, no debug output).
#
# Schema for 'config': Test.Syd.Mutation.Plugin.OptParse.MutationPluginConfig.

{ config ? { } # Plugin configuration; rendered to YAML and passed via --config=PATH
, ghcMemLimit ? "16g" # RTS heap limit for GHC during instrumented compilation (e.g. "8g", "16g")
}:
pkg: # the Haskell package derivation to wrap

let
  configFile = writeText "mutation-config.yaml" (builtins.toJSON config);
  configConfigureFlags =
    if config == { }
    then [ ]
    else [ "--ghc-options=-fplugin-opt=Test.Syd.Mutation.Plugin:--config=${configFile}" ];

  # Extract the names of top-level `executable <name>` stanzas from a Cabal
  # file.  Stanza keywords are case-insensitive; only top-level (column-0)
  # stanzas are considered, which is enough for the packages this is used
  # against.  Returns a list of strings.
  cabalExecutableNames = cabalText:
    let
      lines = lib.splitString "\n" cabalText;
      matchExe = l: builtins.match "[Ee][Xx][Ee][Cc][Uu][Tt][Aa][Bb][Ll][Ee][[:space:]]+([^[:space:]]+).*" l;
      hits = builtins.filter (m: m != null) (map matchExe lines);
    in
    map (m: builtins.head m) hits;

  cabalFile = "${pkg.src}/${pkg.pname}.cabal";
  cabalText =
    if builtins.pathExists cabalFile
    then builtins.readFile cabalFile
    else "";
  executableNames = cabalExecutableNames cabalText;
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
    echo "mutation-nix: setting MUTATION_MANIFEST_DIR=$manifest"
    mkdir -p "$manifest"
    export MUTATION_MANIFEST_DIR="$manifest"
  '';
  # The main 'Setup build' invocation (driven by buildFlags above) has
  # 'lib:${old.pname}' as its buildTarget so only the library is compiled
  # with the plugin.  When the package also declares 'executable' stanzas,
  # we build each of those in postBuild with the plugin loaded but silenced
  # via MUTATION_PLUGIN_SKIP=1.  Test-suites and benchmarks are deliberately
  # NOT built here: 'mutationCheck.nix' wraps test packages with doCheck=true
  # in its own overlay and copies the test executables to $out/test; this
  # avoids duplicating that work and avoids polluting $out/bin with test
  # exes.  We cannot drop the plugin flags from the second build because
  # Cabal would see the package set change and rebuild the library
  # un-instrumented; the runtime kill switch keeps the library's compiled
  # artefacts intact.
  postBuild = (old.postBuild or "") + ''
    echo "mutation-nix: manifest output at $manifest:"
    ls -la "$manifest/" || echo "(empty)"
  '' + lib.optionalString (executableNames != [ ]) ''
    echo "mutation-nix: building executables with plugin silenced: ${lib.concatStringsSep " " executableNames}"
    MUTATION_PLUGIN_SKIP=1 ./Setup build \
      --ghc-option=-fplugin=Test.Syd.Mutation.Plugin \
      --ghc-option=-plugin-package=sydtest-mutation-plugin \
      --ghc-option=-package=sydtest-mutation-plugin \
      ${lib.concatMapStringsSep " " (n: "exe:${n}") executableNames}
  '';
  # The library is installed by nixpkgs's default installPhase (which runs
  # 'Setup copy lib:${pname}').  Executables built in postBuild are not
  # copied by that command, so we install them manually here.  Downstream
  # wrappers such as opt-env-conf's installManpagesAndCompletions expect
  # $out/bin/<exe> to exist when their postInstall hooks run, so this must
  # run BEFORE any existing postInstall hooks the caller has already
  # attached.
  postInstall = lib.optionalString (executableNames != [ ]) ''
    mkdir -p $out/bin
    ${lib.concatMapStringsSep "\n" (n: ''
      src="dist/build/${n}/${n}"
      if [ -f "$src" ] && [ -x "$src" ]; then
        cp "$src" "$out/bin/${n}"
      else
        echo "mutation-nix: expected executable at $src but it is missing" >&2
        exit 1
      fi
    '') executableNames}
  '' + (old.postInstall or "");
})).overrideAttrs (old: {
  outputs = (old.outputs or [ "out" ]) ++ [ "manifest" ];
})
