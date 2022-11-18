# Contributing

1. Run `nix-shell` to install the pre-commit hooks.

2. Make your contribution.

3. Run these successfully.

```
stack clean
stack test --pedantic
nix flake check
```

4. Make a PR to `development`.
5. Make sure CI passes
6. Ask for review
