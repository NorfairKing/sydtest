# sydtest

An experimental testing framework for Haskell with good defaults and advanced testing features.

Sydtest aims to make the common easy and the hard possible.


## The devil is in the defaults

This project chooses best practices as defaults:

- A fixed seed for deterministic randomness
- Pretty show for output
- Bounded shrinking by default

## Advanced testing features

- Multi-line coloured diff for equality assertion failures
- Wrap a test group to use a `withResource`-like function (`aroundAll`)

## Features & Comparison to similar projects

|                                                                                           | sydtest | Hspec                                                       | Tasty                                                            |
|-------------------------------------------------------------------------------------------|---------|-------------------------------------------------------------|------------------------------------------------------------------|
| Declarative test definition                                                               | ✔️       | ✔️                                                           | ✔️                                                                |
| Monadic test definition                                                                   | ✔️       | ✔️                                                           | ✖️                                                                |
| Safe test execution                                                                       | ✔️       | ✔️                                                           | ✔️                                                                |
| Parallel test execution                                                                   | ✔️       | ✔️                                                           | ?                                                                |
| Parallel or sequential test-group execution                                               | ✔️       | ✔️                                                           | ?                                                                |
| Automatic test discovery [1]                                                              | ✔️       | ✔️                                                           | ✔️                                                                |
| First-class support for pure tests                                                        | ✔️       | ✔️                                                           | ✖️                                                                |
| First-class support for integration tests                                                 | ✔️       | ✔️                                                           | [Lib](https://hackage.haskell.org/package/tasty-hunit)           |    
| First-class support for property tests with QuickCheck                                    | ✔️       | ✔️                                                           | [Lib](https://hackage.haskell.org/package/tasty-quickcheck)      |
| First-class support for property tests with Hedgehog                                      | 🚧      | [Lib](https://hackage.haskell.org/package/hspec-hedgehog)   | [Lib](https://hackage.haskell.org/package/tasty-hedgehog)        |
| First-class support for property tests with Smallcheck                                    | 🚧      | [Lib](https://hackage.haskell.org/package/hspec-smallcheck) | [Lib](https://hackage.haskell.org/package/tasty-smallcheck)      |
| First-class support for golden tests                                                      | 🚧      | [Lib](https://hackage.haskell.org/package/hspec-golden)     | [Lib](https://hackage.haskell.org/package/tasty-golden)          |
| Source location annotations for tests in test output                                      | ✔️       | ✔️                                                           | ✖️                                                                |
| Test suite filtering to select which tests to run                                         | ✔️       | ✔️                                                           | ✔️                                                                |
| Individual test execution timing                                                          | 🚧      | [Lib](http://hackage.haskell.org/package/hspec-slow)        | [Lib](https://hackage.haskell.org/package/tasty-stats)           |
| Test group execution timing                                                               | 🚧      | ✖️                                                           | ?                                                                |
| Test suite execution timing                                                               | 🚧      | ✔️                                                           | ✖️                                                                |
| Coloured output                                                                           | ✔️       | ✔️                                                           | ✔️                                                                |
| Colourless output                                                                         | 🚧      | ✔️                                                           | ✔️                                                                |
| `ppShow` instead of `show` for counterexample output                                      | ✔️       | ✖️                                                           | ✖️                                                                |
| `show` for counterexample output                                                          | 🚧      | ✔️                                                           | ✔️                                                                |
| multi-line diffing                                                                        | ✔️       | ✖️                                                           | ✖️                                                                |
| Coloured diffing                                                                          | ✔️       | ✔️                                                           | ✖️                                                                |
| Assertion-specific output with explanation                                                | ✔️       | ✔️                                                           | ?                                                                |
| Fancy Unicode output                                                                      | ✔️       | ✖️                                                           | ✖️                                                                |
| Unicode-free output                                                                       | 🚧      | ✖️                                                           | ✔️                                                                |
| Inter-test progress output during test suite execution                                    | ✔️       | ✔️                                                           | ?                                                                |
| Intra-test progress output during test suite execution                                    | 🚧      | ✔️                                                           | ?                                                                |
| Optional standard output and standard error suppression [2]                               | ✖️       | ✖️                                                           | ✖️                                                                |
| Aquire and release a resource for every test in a group (`before` and `after`)            | ✔️       | ✔️                                                           | ✖️                                                                |
| Aquire and release a resource once for an entire test group (`beforeAll` and `afterAll`)  | ✔️       | ✔️                                                           | ✔️                                                                |
| Wrap a single test to use a `withResource`-like function (`around`)                       | ✔️       | ✔️                                                           | ✖️                                                                |
| Wrap a test group to use a `withResource`-like function (`aroundAll`)                     | ✔️       | ✖️                                                           | ✖️                                                                |
| Randomising execution order                                                               | 🚧      | ✔️                                                           | ?                                                                |
| Randomised execution order by default                                                     | 🚧      | ✖️                                                           | ?                                                                |
| Deterministic randomness                                                                  | ✔️       | ✔️                                                           | ✔️                                                                |
| Deterministic randomness by default                                                       | ✔️       | ✖️                                                           | ✖️                                                                |
| Deterministic randomness instructions for rerunning tests                                 | ✔️       | ✔️                                                           | ?                                                                |
| Nice process by default                                                                   | ✔️       | ✖️                                                           | ✖️                                                                |
| Hiding process arguments from tests                                                       | 🚧      | ✔️                                                           | ?                                                                |
| Declaring that an individual test should fail                                             | 🚧      | ✖️                                                           | [Lib](http://hackage.haskell.org/package/tasty-expected-failure) |
| Declaring that at least one in a test group should fail                                   | 🚧      | C                                                           | [Lib](http://hackage.haskell.org/package/tasty-expected-failure) |
| Using scarce resources across tests                                                       | 🚧      | C                                                           | ?                                                                |
| A way to fail the test suite as soon as one test fails (`--fail-fast`)                    | 🚧      | ✔️                                                           | ?                                                                |

* ✔️: Supported 
* Lib: Possible with an extra library
* C: Possible but you have to write some code yourself
* 🚧 — Under development
* ✖️: Not supported
* ?: I don't know.

Please let me know if I made a mistake anywhere, and feel free to fill in the question marks

* [1]: Test discovery is always handled via a separate library so I use `✔️` instead of `Lib`.
* [2]: It turns out that this is surprisingly difficult, due to [forkProcess' interaction with `MVar`s](https://www.reddit.com/r/haskell/comments/jsap9r/how_dangerous_is_forkprocess/) but I'm still looking for a way to make it happen. The answer may lie in [the way `weigh` does it](https://github.com/fpco/weigh/blob/bfcf4415144d7d2817dfcb91b6f9a6dfd7236de7/src/Weigh.hs#L373)


## Features in detail

### Declarative monadic test definition

Tests are declared as follows:

``` haskell
spec :: Spec
spec = do
  describe "myFunction" $ do -- description of which functions you are testing
    it "does what you want it to" $ -- sentence to describe what you expect to happen
      2 + 3 == 5 -- Test code
```

### Safe test execution

Code that throws exceptions can be tested without trouble:

``` haskell
spec :: Spec
spec = do
  describe "pred" $ do
    it "throws no exceptions" $
      pred (0 :: Word) -- causes overflow (below zero), so this test will fail.
```

### Parallel test execution

Tests are executed with as many threads as you have capabilities by default.
You can use `-j` or `--jobs` to set the number of threads to use.
