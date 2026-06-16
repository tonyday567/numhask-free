# Changelog

## 0.2.0.0

- Refactored `NumHask.Diff` to use `NumHask.Prelude` instead of qualified imports.
- Removed the `Num` instance for `Diff'`.
- Added `Eq`, lattice, bounded, action, `Basis`, and `Direction` instances for `Diff'`.
- Added `FromInteger` and `FromRational` instances for literal support.
- Added branch-visible non-smooth primitives: `maxWith`, `minWith`, `absWith`, `signumWith`.

## 0.1.0.0

- Initial release
- `NumHask.Free.Additive` — free commutative monoid
