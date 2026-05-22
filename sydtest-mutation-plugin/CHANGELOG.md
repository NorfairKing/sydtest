# Changelog

## [0.1.0.0] - 2026-05-22

### Added

* A `DisableMutationsFor <name>` (or `DisableMutationFor <name>`) annotation
  whose target names no local binding in the annotated function's body is now
  a compile error.  Such an annotation disables no mutations, so the plugin
  asks for its removal.
