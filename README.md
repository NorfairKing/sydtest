# sydtest

An experimental testing framework for Haskell

## Features & Comparison to similar projects

|                                                                       | sydtest | Hspec                                                       | Tasty                                                            |
|-----------------------------------------------------------------------|---------|-------------------------------------------------------------|------------------------------------------------------------------|
| Declarative test definition                                           | ✔️       | ✔️                                                           | ✔️                                                                |
| Monadic test definition                                               | ✔️       | ✔️                                                           | ✖️                                                                |
| Safe test execution                                                   | ✔️       | ✔️                                                           | ✔️                                                                |
| First-class support for pure tests                                    | ✔️       | ✔️                                                           | ✖️                                                                |
| First-class support for integration tests                             | ✔️       | ✔️                                                           | [Lib](https://hackage.haskell.org/package/tasty-hunit)           |    
| First-class support for property tests with QuickCheck                | ✔️       | ✔️                                                           | [Lib](https://hackage.haskell.org/package/tasty-quickcheck)      |
| First-class support for property tests with Hedgehog                  | 🚧      | [Lib](https://hackage.haskell.org/package/hspec-hedgehog)   | [Lib](https://hackage.haskell.org/package/tasty-hedgehog)        |
| First-class support for property tests with Smallcheck                | 🚧      | [Lib](https://hackage.haskell.org/package/hspec-smallcheck) | [Lib](https://hackage.haskell.org/package/tasty-smallcheck)      |
| Source location annotations for tests in test output                  | ✔️       | ✔️                                                           | ✖️                                                                |
| Individual test execution timing                                      | 🚧      | ✖️                                                           | [Lib](https://hackage.haskell.org/package/tasty-stats)           |
| Test suite execution timing                                           | 🚧      | ✔️                                                           | ✖️                                                                |
| Coloured output                                                       | ✔️       | ✔️                                                           | ✔️                                                                |
| Colourless output                                                     | 🚧      | ✔️                                                           | ✔️                                                                |
| `ppShow` instead of `show` for counterexample output                  | ✔️       | ✖️                                                           | ✖️                                                                |
| multi-line diffing                                                    | ✔️       | ✖️                                                           | ✖️                                                                |
| Coloured diffing                                                      | ✔️       | ✔️                                                           | ✖️                                                                |
| Fancy Unicode output                                                  | ✔️       | ✖️                                                           | ✖️                                                                |
| Unicode-free output                                                   | 🚧      | ✖️                                                           | ✔️                                                                |
| Inter-test progress output during test suit execution                 | ✔️       | ✔️                                                           |                                                                  |
| Intra-test progress output during test suit execution                 | 🚧      | ✔️                                                           |                                                                  |
| Optional standard output and standard error suppression               | ✖️       | ✖️                                                           | ✖️                                                                |
| Aquire and release a resource for every test in a group               | ✔️       | ✔️                                                           | ✖️                                                                |
| Aquire and release a resource once for an entire test group           | ✔️       | ✔️                                                           | ✔️                                                                |
| Wrap a single test to use a withResource function                     | ✔️       | ✔️                                                           | ✖️                                                                |
| Wrap a test group to use a withResource function                      | ✔️       | ✖️                                                           | ✖️                                                                |

* ✔️: Supported 
* L: Possible with an extra library
* C: Possible but you have to write some code yourself
* 🚧 — Under development
* ✖️: Not supported

Please let me know if I made a mistake anywhere
