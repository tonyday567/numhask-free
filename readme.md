numhask-free
===

Initial encoding of numhask's numeric hierarchy.

**syntax versus semantics** ⟜ constructors versus observation
**terms versus classes** ⟜ free algebras versus tagless-final

The dual of numhask: where numhask uses type classes (tagless-final),
numhask-free uses data types (initial encoding). Every class becomes a
data type; every method becomes a constructor. `eval` is the unique
homomorphism out of the free object — the reify triangle, one
dimension down.

Hierarchy
---

- `NumHask.Free.Additive` — free commutative monoid

Scope
---

Terms exist freely through Ring. Beyond Ring, the theory is not a
variety: `Divisive`'s `recip` is partial, conditional, not equational.
The naive recursion stops there. The fix is either localization
(formal fractions with nonzero witnesses) or the relational move
(mirrors / duplex), but never a naive `Recip` constructor.
