# Changelog

## [0.4.1.0] - 2026-06-19

### Fixed

* Mutants whose replacement text contains a `/` are now activated (and so
  killable) instead of always surviving.  A `MutationId` renders and parses as
  `/`-separated parts, so its parts must contain no `/`, yet the id embedded the
  replacement string (raw source text) as a part.  A `SwitchFunctionArguments`
  swap of e.g. `stripPrefix "/nix/store/" t` has replacement text
  `t "/nix/store/"`, which contains a `/`.  The compiled `ifMutation` carried
  that id intact, but the driver renders the id into `MUTATION_ACTIVE` and the
  runtime parses it back by splitting on `/`: the embedded `/` split into extra
  parts, the parsed id never equalled the compiled one, and the mutation was
  never made active.  It therefore survived even when a test killed it, while
  same-site operators with a `/`-free replacement (e.g. `ConstNothing`'s
  `Nothing`) were killed normally.  (The coverage phase still recorded the
  intact id, so the mutant showed up as `survived`, not `uncovered`.)  The id
  is now the structural key `[module, operator, line, colStart, colEnd,
  altIndex]`; none of those parts can contain a `/`, and the index already
  disambiguates alternatives at one span.  The replacement text remains in the
  manifest's `replacement` field for humans.

## [0.4.0.0] - 2026-06-17

### Added

* A `RemoveClause` mutation operator: for a function binding with two or more
  equations, it emits one mutant per equation that removes that equation.
  Because a clause is a binding-level `Match` rather than an expression, the
  removal is implemented by prepending an `ifMutation mid False True` guard to
  the clause: when the mutation is active every guard of the clause fails, so
  the clause matches no input and control falls through to the next equation
  (or to a non-exhaustive `MatchFail` for the last matching clause).  The guard
  sits after the clause's patterns, so coverage is attributed only to tests
  that actually reach the clause.  Disable it like any other operator via
  `disabled-mutations`, a module `{-# ANN #-}`, or
  `operators.RemoveClause.enable: false`.

* A `SwitchFunctionArguments` mutation operator: at a prefix function (or
  constructor) application, for every pair of value arguments that have the
  same type, it emits a mutant that swaps those two arguments.  Only the
  maximal application spine is mutated, and pairs whose arguments have
  identical source text are skipped (swapping them would be a no-op).

  Symmetric functions make this operator produce equivalent (unkillable)
  mutants, so it reads a `skip-calls-to` list of function names from its
  per-operator config and skips swapping the arguments of any call to a
  listed function:

  ```yaml
  operators:
    SwitchFunctionArguments:
      skip-calls-to: [max, Data.Set.union]
  ```

* Mutations now carry an optional `mitigation` hint and the enclosing
  `binding` name in the manifest.  `SwitchFunctionArguments` sets a mitigation
  message pointing at `skip-calls-to`; the binding name lets the run report
  print the exact `{-# ANN <binding> ("DisableMutation: <Operator>" ...) #-}`
  annotation for each surviving mutation.

## [0.3.0.0] - 2026-06-09

### Added

* Per-operator configuration under the `operators` config object:

  ```yaml
  operators:
    ConstEmptyList:
      skip-strings: true
    Arith:
      enable: false
  ```

  Every operator takes an `enable` toggle (`enable: false` disables it, the
  same as listing it in `disabled-mutations`).  Each operator reads its own
  remaining keys, exposed as an opaque `operatorConfigExtra :: Map Text Value`.
  The `ConstEmptyList` operator interprets two such keys: `skip-strings`
  (do not target `[Char]`/`String` expressions at all — keeps genuine `[a]`,
  `a /= Char`, list mutations) and `skip-literal-strings` (skip only
  syntactic string literals).

### Changed

* `InstrumentEnv` and `runInstrument` now carry the per-operator
  configuration (`Map Text OperatorConfig`) rather than individual flags.
* The `OptParse` settings expose `OperatorConfig`, `operatorsConfigDisabled`,
  and `operatorExtraFlag`; the old `OperatorsConfig`/`ConstEmptyListConfig`
  types are gone.

## [0.2.1.0] - 2026-06-08

### Fixed

* The instrumenter no longer mutates compiler-generated code.  Stock- and
  anyclass-derived instance methods (whose `MatchGroup` carries a `Generated`
  origin) and the dictionary/evidence `VarBind`s the typechecker materialises
  for an instance (e.g. `$dShow`, `$dEnum`) were being instrumented, with
  source spans pointing at the `deriving` clause or the instance head.  That
  produced nonsense diffs like `deriving (Show, (\_ _ -> False), ...)` and
  `instance (\_ -> []) where`, and the resulting mutants — living in generated
  code rather than the user's own source — could never be killed by a test, so
  they surfaced as permanently uncovered mutations.  Such bindings are now
  skipped.

## [0.2.0.0] - 2026-06-04

### Added

* Three const-function mutation operators that replace an expression of
  type `arg1 -> ... -> argN -> T` (with `N >= 0`) by a constant function
  returning `T`'s distinguished trivial value: `ConstNothing` for `Maybe a`,
  `ConstEmptyList` for `[a]`, and an expanded `ConstBool` for `Bool`
  (subsuming the previous arity-0-only `ConstBool` behaviour).  At arity 0
  the mutant is the bare trivial value; at arity `N >= 1` it is a typed
  `(\_ ... _ -> v)` lambda synthesized in the GhcTc AST.  Maybe and List
  thereby gain coverage that the syntactic-only `MaybeOp` and `ListLit`
  miss — calls like `lookup k m` and `concat xs` are now mutated to
  `Nothing` and `[]` respectively.
* A coloured human-readable `<Module>.txt` rendering of every manifest is
  now written alongside the existing canonical `<Module>.json`.  Same
  header + unified-diff layout as the runtime's surviving-mutation
  report, opening with a `N mutations in M groups` count line so empty
  manifests are not mistakeable for rendering accidents.
* `ReplaceOuterSpan RealSrcSpan Text` constructor on `SrcSpanDelta` for
  operators that want to replace a wider outer span than the matched
  expression's own.  Used by the const-family operators when the matched
  expression sits at the operator-token position of an infix application
  to rewrite the whole `OpApp` source span in prefix form (e.g.
  `n < 0` becomes `(\_ _ -> True) (n) (0)`) instead of text-splicing a
  lambda into the operator slot and producing nonsense source.

### Changed

* `applyTokenReplace` now collapses multi-line spans correctly (a latent
  bug that surfaced once arity-`>=1` operators started emitting multi-line
  replacements).
* The walker tracks `HsApp` function-position depth on `InstrumentEnv`,
  resetting on the argument side.  Const-family operators consult it to
  skip arity-`N` firings (`N >= 1`) at sites where the matched expression
  is already saturated by `N` enclosing applications, because the
  arity-`N` mutant evaluates to what the arity-0 mutation on the
  outermost saturated expression already produces.

## [0.1.1.0] - 2026-05-25

### Changed

* Mutations whose desugared form is non-exhaustive (e.g. a `RemoveCase`
  that drops the only alternative covering a constructor) are now
  auto-killed at instrument time, since the compiler's incomplete-patterns
  warning would catch them anyway.

## [0.1.0.0] - 2026-05-22

### Added

* A `DisableMutationsFor <name>` (or `DisableMutationFor <name>`) annotation
  whose target names no local binding in the annotated function's body is now
  a compile error.  Such an annotation disables no mutations, so the plugin
  asks for its removal.
