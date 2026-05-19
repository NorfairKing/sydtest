{ haskell, haskellPackages }:

# Allow linking against sydtest-mutation-runtime.

pkg:

haskell.lib.overrideCabal pkg (old: {
  # In order to avoid linker errors, every package that depends on a package
  # instrumented with the mutation plugin must also be linked against
  # sydtest-mutation-runtime.
  # We add the appropriate buildFlags and buildDepends here because it's
  # usually good enough.
  # That is to say we usually definitely have to add these buildFlags and
  # buildDepends here, but some dependencies might be building executables
  # (e.g. benchmarks or test suites) that won't automatically be linked
  # against sydtest-mutation-runtime.
  # For example, consider the case where we have three packages:
  #
  # A -> B -> C
  #
  # We instrument C, and we add the runtime dep to A so that linking A's test
  # suite works.
  # However, if B has a benchmark or test suite, it will still fail to link.
  buildFlags = (old.buildFlags or [ ]) ++ [
    # build-depends in the to-instrument haskell package's cabal file.
    "--ghc-option=-package=sydtest-mutation-runtime"
    # Turn off any unused packages warnings that the package might have
    # enabled. In case sydtest-mutation-runtime might not be used directly.
    "--ghc-option=-Wno-unused-packages"
  ];
  # We use libraryHaskellDepends instead of buildDepends because that's what
  # cabal2nix generates, see default.nix in any of the package directories, but
  # I don't think it actually matters because `libraryHaskellDepends` is
  # combined with `buildDepends` everywhere it is used within this code:
  # https://github.com/NixOS/nixpkgs/blob/7a6a010c3a1d00f8470a5ca888f2f927f1860a19/pkgs/development/haskell-modules/generic-builder.nix#L18.
  libraryHaskellDepends = (old.libraryHaskellDepends or [ ]) ++ [
    haskellPackages.sydtest-mutation-runtime
  ];
})
