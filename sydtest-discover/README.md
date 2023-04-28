# Sydtest Discover

This package exists to automate finding all your test suites and running them from a single entry point.

## Basic setup

The most basic operation consists of defining a test suite in your `package.yaml` like this:

``` yaml
  foobar-test:
    main: Spec.hs
    source-dirs: test/
    build-tools: sydtest-discover
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - sydtest
```

You put this in your `Spec.hs`:

``` haskell
{-# OPTIONS_GHC -F -pgmF sydtest-discover #-}
```

You don't _need_ to know what this means, but just in case you're curious: This has `ghc` call `sydtest-discover` here and puts the results in this file (sort-of).

Now you can write test files (modules ending in `Spec`) in your `test` directory and they will be picked up as long as you have one top-level `spec :: Spec` function in it:

``` haskell
spec :: Spec
spec = 
  describe "(+)" $ 
    specify "2 + 2 = 4" $ 
      2 + 2 `shouldBe` 4
```

## Custom main

Sometimes you still want to collect all the `spec`s together but you want to run your own main function instead of just calling `sydTest :: Spec -> IO ()` on them.
You may want to use `sydTestWith :: Test.Syd.Settings -> Spec -> IO ()` for example.

In that case your `package.yaml` will look like this:

``` yaml
  foobar-test:
    main: Main.hs # <- This is different now
    source-dirs: test/
    build-tools: sydtest-discover
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - sydtest
```

You will put this in your `Spec.hs` (not the `Main.hs`):

``` haskell
{-# OPTIONS_GHC -F -pgmF sydtest-discover -optF --no-main #-}
```

Now you can define your own `Main.hs`:

``` haskell
module Main where

import Spec
import Test.Syd

main :: IO ()
main = sydTest spec
```

Note that `sydtest-discover` does not require that your top-level `spec` is of type `Spec`.
That means that you could define a single outer resource for your entire test suite, and only construct it at the very top level in `Main.hs`.

## Combining several usages of sydtest-discover

There might be cases where you have a test suite where one part of the tests has different requirements than the rest, e.g. outer resources.
To make that work, you can call `sydtest-discover` in multiple places and then combine the discovered test suites in a custom main function.
Your test suite could be structured like this

``` text
test
├── Bar
│   ├── BarSpec.hs
│   └── Spec.hs
├── Foo
│   ├── FooSpec.hs
│   └── Spec.hs
└── Main.hs
```

where the content of `Spec.hs` in `Foo` and `Bar` looks like that:

``` haskell
{-# OPTIONS_GHC -F -pgmF sydtest-discover -optF --no-main #-}
```

In `Main.hs` you then call both discovered test suites:

``` haskell
module Main where

import qualified Bar.Spec as Bar
import qualified Foo.Spec as Foo
import Test.Syd

main :: IO ()
main = sydTest $ do
  Foo.spec
  Bar.spec
```

For the test suites that need a special setup, you can add it here using `beforeAll` and friends.

The `multi-discover-test` test suite in the `sydtest` module demonstrates this use case.
