{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}

-- | Reverse-mode automatic differentiation as a NumHask carrier.
--
-- A 'Diff s b' is a smooth function @s -> b@ bundled with its pullback.
-- These instances turn it into a NumHask carrier in its own right, so any
-- function written with NumHask-polymorphic operators becomes differentiable
-- by instantiating at @Diff s b@.  The derivative rules live exactly where
-- they should: as the instance methods.
--
-- This is the "functorial lift" direction of the ecosystem membrane:
-- NumHask-polymorphic code wraps AD by using 'Diff' as its carrier.
--
-- The lift in one doctest: the identity primitive is "the variable", every
-- operator applied to it is an instance method carrying its own derivative,
-- and the pullback of the composite is the chain rule assembled by
-- instance resolution.  @f s = sin s^2 + s^3@ at @s = 2@: value
-- @sin 4 + 8@, gradient @4*cos 4 + 12@.
--
-- >>> import NumHask.Diff (Diff, runDiff)
-- >>> import NumHask.Algebra.Additive qualified as NHA
-- >>> import NumHask.Algebra.Multiplicative qualified as NHM
-- >>> import NumHask.Algebra.Field qualified as NHF
-- >>> let x = Diff (\s -> (s, \db -> db)) :: Diff Double Double
-- >>> let f = NHF.sin (x NHM.* x) NHA.+ x NHM.* x NHM.* x
-- >>> let (y, pb) = runDiff f 2.0
-- >>> abs (y - (sin 4 + 8)) < 1e-12
-- True
-- >>> abs (pb 1.0 - (4 * cos 4 + 12)) < 1e-12
-- True
module NumHask.Diff
  ( Diff,
    Diff' (..),
    pattern Diff,
    runDiff,

    -- * Branch-visible non-smooth primitives
    maxWith,
    minWith,
    absWith,
    signumWith,
  )
where

import NumHask.Prelude
import Prelude ()

-- | A reverse-mode differentiable function tagged by a phantom type @p@.
--
-- The phantom tag prevents perturbation confusion: values of type
-- @Diff' p a b@ can only be composed with other @Diff' p@ values.  Nested
-- AD introduces a fresh tag for each level.
--
-- @runDiff f a@ returns a pair @(b, pullback)@ where @b = f a@ and @pullback@
-- maps a cotangent @db@ on the output to a cotangent @da@ on the input.
newtype Diff' (p :: k) a b = Diff'
  { -- | Run the forward pass and return the backward pullback.
    runDiff' :: a -> (b, b -> a)
  }

-- | The untagged differentiable arrow.  Existing code can continue to use
-- this; it is simply @Diff' ()@.
type Diff = Diff' ()

-- | Pattern synonym for the untagged constructor.  Use this or 'Diff'' to
-- build a 'Diff'' value.
pattern Diff :: (a -> (b, b -> a)) -> Diff' p a b
pattern Diff f = Diff' f

{-# COMPLETE Diff :: Diff' #-}

-- | Run the forward pass and return the backward pullback.
runDiff :: Diff' p a b -> a -> (b, b -> a)
runDiff = runDiff'

instance Category (Diff' p) where
  id = Diff (\a -> (a, id))
  Diff f . Diff g = Diff $ \a ->
    let (b, gb) = g a
        (c, fc) = f b
     in (c, gb . fc)

-- | Additive structure: sum rule.
--
-- @zero@ is the constant zero function; @(+)@ adds outputs and fans-in
-- cotangents.
instance (Additive s, Additive b) => Additive (Diff' p s b) where
  zero = Diff (\_ -> (zero, const zero))

  Diff f + Diff g = Diff $ \s ->
    let (b1, p1) = f s
        (b2, p2) = g s
     in (b1 + b2, \db -> p1 db + p2 db)

-- | Subtractive structure: negation pushes through the pullback.
instance (Additive s, Subtractive s, Subtractive b) => Subtractive (Diff' p s b) where
  negate (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (negate b, \db -> negate (p db))

  Diff f - Diff g = Diff $ \s ->
    let (b1, p1) = f s
        (b2, p2) = g s
     in (b1 - b2, \db -> p1 db - p2 db)

-- | Multiplicative structure: product rule.
--
-- @one@ is the constant one function.
instance (Additive s, Multiplicative b) => Multiplicative (Diff' p s b) where
  one = Diff (\_ -> (one, const zero))

  Diff f * Diff g = Diff $ \s ->
    let (b1, p1) = f s
        (b2, p2) = g s
     in (b1 * b2, \db -> p1 (db * b2) + p2 (b1 * db))

-- | Divisive structure: reciprocal rule.
--
-- Division inherits the product rule via the default @'/' = '*' . 'recip'@.
instance
  (Additive s, Subtractive b, Multiplicative b, Divisive b) =>
  Divisive (Diff' p s b)
  where
  recip (Diff f) = Diff $ \s ->
    let (b, p) = f s
        r = recip b
        rr = r * r
     in (r, \db -> p (negate (db * rr)))

-- | Exponential field: @exp@, @log@, and the derived power/root family.
instance
  (ExpField b, Additive s, Subtractive s, Multiplicative b) =>
  ExpField (Diff' p s b)
  where
  exp (Diff f) = Diff $ \s ->
    let (b, p) = f s
        e = exp b
     in (e, \db -> p (db * e))

  log (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (log b, \db -> p (db * recip b))

-- | Trigonometric field: the elementary transcendental family.
instance
  ( TrigField b,
    ExpField b,
    Additive s,
    Subtractive s,
    Multiplicative b,
    Divisive b
  ) =>
  TrigField (Diff' p s b)
  where
  pi = Diff (\_ -> (pi, const zero))

  sin (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (sin b, \db -> p (db * cos b))

  cos (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (cos b, \db -> p (negate (db * sin b)))

  asin (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = recip (sqrt (one - b * b))
     in (asin b, \db -> p (db * d))

  acos (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = recip (sqrt (one - b * b))
     in (acos b, \db -> p (negate (db * d)))

  atan (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = recip (one + b * b)
     in (atan b, \db -> p (db * d))

  atan2 (Diff f) (Diff g) = Diff $ \s ->
    let (y, py) = f s
        (x, px) = g s
        r = atan2 y x
        denom = y * y + x * x
        dy = x / denom
        dx = negate (y / denom)
     in (r, \db -> py (db * dy) + px (db * dx))

  sinh (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (sinh b, \db -> p (db * cosh b))

  cosh (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (cosh b, \db -> p (db * sinh b))

  asinh (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = recip (sqrt (b * b + one))
     in (asinh b, \db -> p (db * d))

  acosh (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = recip (sqrt (b * b - one))
     in (acosh b, \db -> p (db * d))

  atanh (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = recip (one - b * b)
     in (atanh b, \db -> p (db * d))

-- | Equality is not decidable for differentiable arrows, but a vacuous
-- 'Eq' instance is provided so that 'JoinSemiLattice' and 'MeetSemiLattice'
-- can be lifted.  Treat '==' as always false.
instance Eq (Diff' p s b) where
  _ == _ = False

-- | Lattice structure lifted pointwise.  Join is minimum, meet is maximum
-- (matching 'NumHask.Algebra.Lattice' for ordered scalar types).  Ties
-- choose the left argument; at ties the derivative is discontinuous.
instance (Ord b) => JoinSemiLattice (Diff' p s b) where
  Diff f \/ Diff g = Diff $ \s ->
    let (x, px) = f s
        (y, py) = g s
     in if x <= y then (x, px) else (y, py)

instance (Ord b) => MeetSemiLattice (Diff' p s b) where
  Diff f /\ Diff g = Diff $ \s ->
    let (x, px) = f s
        (y, py) = g s
     in if x >= y then (x, px) else (y, py)

instance (Ord b, LowerBounded b, Additive s) => LowerBounded (Diff' p s b) where
  bottom = Diff (\_ -> (bottom, const zero))

instance (Ord b, UpperBounded b, Additive s) => UpperBounded (Diff' p s b) where
  top = Diff (\_ -> (top, const zero))

-- | Additive action by a constant scalar.
instance (AdditiveAction b) => AdditiveAction (Diff' p s b) where
  type AdditiveScalar (Diff' p s b) = AdditiveScalar b
  Diff f |+ k = Diff $ \s ->
    let (x, p) = f s
     in (x |+ k, p)

-- | Subtractive action by a constant scalar.
instance (SubtractiveAction b) => SubtractiveAction (Diff' p s b) where
  Diff f |- k = Diff $ \s ->
    let (x, p) = f s
     in (x |- k, p)

-- | Multiplicative action by a constant scalar.
instance (MultiplicativeAction b) => MultiplicativeAction (Diff' p s b) where
  type Scalar (Diff' p s b) = Scalar b
  Diff f |* k = Diff $ \s ->
    let (x, p) = f s
     in (x |* k, \dz -> p (dz |* k))

-- | Divisive action by a constant scalar.
instance (DivisiveAction b) => DivisiveAction (Diff' p s b) where
  Diff f |/ k = Diff $ \s ->
    let (x, p) = f s
     in (x |/ k, \dz -> p (dz |/ k))

-- | Basis structure for endo-based carriers.  'magnitude' differentiates
-- through 'basis'; 'basis' itself is treated as piecewise constant (zero
-- pullback).  At the kink this is only a subgradient.
instance
  (Basis b, Mag b ~ b, Base b ~ b, Additive s) =>
  Basis (Diff' p s b)
  where
  type Mag (Diff' p s b) = Diff' p s b
  type Base (Diff' p s b) = Diff' p s b
  magnitude (Diff f) = Diff $ \s ->
    let (x, p) = f s
     in (magnitude x, \dm -> p (basis x * dm))
  basis (Diff f) = Diff $ \s ->
    let (x, _) = f s
     in (basis x, const zero)

-- | Direction for 'EuclideanPair'.
--
-- 'angle' differentiates through @atan2 y x@; 'ray' differentiates through
-- @(cos t, sin t)@.  Both are undefined at the origin.
instance (TrigField a, Additive s) => Direction (Diff' p s (EuclideanPair a)) where
  type Dir (Diff' p s (EuclideanPair a)) = Diff' p s a
  angle (Diff f) = Diff $ \s ->
    let (EuclideanPair (x, y), p) = f s
        r2 = x * x + y * y
     in (atan2 y x, \dt -> p (EuclideanPair (negate y * dt / r2, x * dt / r2)))
  ray (Diff f) = Diff $ \s ->
    let (t, p) = f s
        c = cos t
        sn = sin t
     in (EuclideanPair (c, sn), \dxy ->
          let EuclideanPair (dx, dy) = dxy
           in p (dx * negate sn + dy * c))

-- | Direction for 'Complex'.
--
-- Same geometry as 'EuclideanPair'; 'Complex' already has a 'Direction'
-- instance via 'EuclideanPair', so we mirror the derivative here.
instance (TrigField a, Additive s) => Direction (Diff' p s (Complex a)) where
  type Dir (Diff' p s (Complex a)) = Diff' p s a
  angle (Diff f) = Diff $ \s ->
    let (Complex (x, y), p) = f s
        r2 = x * x + y * y
     in (atan2 y x, \dt -> p (Complex (negate y * dt / r2, x * dt / r2)))
  ray (Diff f) = Diff $ \s ->
    let (t, p) = f s
        c = cos t
        sn = sin t
     in (Complex (c, sn), \dxy ->
          let Complex (dx, dy) = dxy
           in p (dx * negate sn + dy * c))

-- | Literal support via 'FromInteger'.  With 'RebindableSyntax' enabled,
-- integer literals like @1@ can be used at type @Diff s b@.
instance (FromInteger b, Additive s) => FromInteger (Diff' p s b) where
  fromInteger n = Diff (\_ -> (fromInteger n, const zero))

-- | Literal support via 'FromRational'.  With 'RebindableSyntax' enabled,
-- decimal literals like @2.0@ can be used at type @Diff s b@.
instance (FromRational b, Additive s) => FromRational (Diff' p s b) where
  fromRational r = Diff (\_ -> (fromRational r, const zero))

-- ---------------------------------------------------------------------------
-- Branch-visible non-smooth primitives
-- ---------------------------------------------------------------------------

-- | Maximum with a visible branch decision.
--
-- Returns the larger value and a boolean that is 'True' exactly when the
-- left argument was chosen (ties choose the left).
--
-- The pullback routes the output cotangent to the active argument; the
-- boolean is returned purely for inspection and does not receive a
-- cotangent.
maxWith ::
  (Ord b) =>
  Diff' p s b ->
  Diff' p s b ->
  Diff' p s (b, Bool)
maxWith (Diff f) (Diff g) = Diff $ \s ->
  let (x, px) = f s
      (y, py) = g s
      c = x >= y
      z = case c of True -> x; False -> y
   in ( (z, c),
        \(dz, _) -> case c of True -> px dz; False -> py dz
      )

-- | Minimum with a visible branch decision.
--
-- Returns the smaller value and a boolean that is 'True' exactly when the
-- left argument was chosen (ties choose the left).
minWith ::
  (Ord b) =>
  Diff' p s b ->
  Diff' p s b ->
  Diff' p s (b, Bool)
minWith (Diff f) (Diff g) = Diff $ \s ->
  let (x, px) = f s
      (y, py) = g s
      c = x <= y
      z = case c of True -> x; False -> y
   in ( (z, c),
        \(dz, _) -> case c of True -> px dz; False -> py dz
      )

-- | Absolute value with a visible sign decision.
--
-- Returns @('abs' x, x >= 0)@.  At the kink @x = 0@ the boolean is 'True'
-- by convention; the caller can inspect it and supply a custom subgradient
-- if needed.
absWith ::
  (Ord b, Subtractive b, Multiplicative b) =>
  Diff' p s b ->
  Diff' p s (b, Bool)
absWith (Diff f) = Diff $ \s ->
  let (x, p) = f s
      c = x >= zero
      y = case c of True -> x; False -> negate x
      u = case c of True -> one; False -> negate one
   in ((y, c), \(dz, _) -> p (dz * u))

-- | Signum with a visible sign decision.
--
-- Returns @(sign x, x >= 0)@ where @sign x@ is 'one' for non-negative @x@
-- and 'negate one' for negative @x@.  The pullback is zero because 'signum'
-- is piecewise constant.
signumWith ::
  (Ord b, Subtractive b, Multiplicative b, Additive s) =>
  Diff' p s b ->
  Diff' p s (b, Bool)
signumWith (Diff f) = Diff $ \s ->
  let (x, _) = f s
      c = x >= zero
      y = case c of True -> one; False -> negate one
   in ((y, c), const zero)
