# Changelog

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
