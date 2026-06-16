{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
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
  )
where


import Control.Category



import NumHask.Algebra.Additive qualified as NHA
import NumHask.Algebra.Field qualified as NHF
import NumHask.Algebra.Multiplicative qualified as NHM
import Prelude hiding (id, (.))
import Prelude qualified as P

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
instance (NHA.Additive s, NHA.Additive b) => NHA.Additive (Diff' p s b) where
  zero = Diff (const (NHA.zero, const NHA.zero))

  Diff f + Diff g = Diff $ \s ->
    let (b1, p1) = f s
        (b2, p2) = g s
     in (b1 NHA.+ b2, \db -> p1 db NHA.+ p2 db)

-- | Subtractive structure: negation pushes through the pullback.
instance (NHA.Additive s, NHA.Subtractive s, NHA.Subtractive b) => NHA.Subtractive (Diff' p s b) where
  negate (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (NHA.negate b, NHA.negate . p)

  Diff f - Diff g = Diff $ \s ->
    let (b1, p1) = f s
        (b2, p2) = g s
     in (b1 NHA.- b2, \db -> p1 db NHA.- p2 db)

-- | Multiplicative structure: product rule.
--
-- @one@ is the constant one function.
instance (NHA.Additive s, NHM.Multiplicative b) => NHM.Multiplicative (Diff' p s b) where
  one = Diff (const (NHM.one, const NHA.zero))

  Diff f * Diff g = Diff $ \s ->
    let (b1, p1) = f s
        (b2, p2) = g s
     in (b1 NHM.* b2, \db -> p1 (db NHM.* b2) NHA.+ p2 (b1 NHM.* db))

-- | Divisive structure: reciprocal rule.
--
-- Division inherits the product rule via the default @'/' = '*' . 'recip'@.
instance
  (NHA.Additive s, NHA.Subtractive b, NHM.Multiplicative b, NHM.Divisive b) =>
  NHM.Divisive (Diff' p s b)
  where
  recip (Diff f) = Diff $ \s ->
    let (b, p) = f s
        r = NHM.recip b
        rr = r NHM.* r
     in (r, \db -> p (NHA.negate (db NHM.* rr)))

-- | Exponential field: @exp@, @log@, and the derived power/root family.
instance
  (NHF.ExpField b, NHA.Additive s, NHA.Subtractive s, NHM.Multiplicative b) =>
  NHF.ExpField (Diff' p s b)
  where
  exp (Diff f) = Diff $ \s ->
    let (b, p) = f s
        e = NHF.exp b
     in (e, \db -> p (db NHM.* e))

  log (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (NHF.log b, \db -> p (db NHM.* NHM.recip b))

-- | Trigonometric field: the elementary transcendental family.
instance
  ( NHF.TrigField b,
    NHF.ExpField b,
    NHA.Additive s,
    NHA.Subtractive s,
    NHM.Multiplicative b,
    NHM.Divisive b
  ) =>
  NHF.TrigField (Diff' p s b)
  where
  pi = Diff (const (NHF.pi, const NHA.zero))

  sin (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (NHF.sin b, \db -> p (db NHM.* NHF.cos b))

  cos (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (NHF.cos b, \db -> p (NHA.negate (db NHM.* NHF.sin b)))

  asin (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = NHM.recip (NHF.sqrt (NHM.one NHA.- b NHM.* b))
     in (NHF.asin b, \db -> p (db NHM.* d))

  acos (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = NHM.recip (NHF.sqrt (NHM.one NHA.- b NHM.* b))
     in (NHF.acos b, \db -> p (NHA.negate (db NHM.* d)))

  atan (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = NHM.recip (NHM.one NHA.+ b NHM.* b)
     in (NHF.atan b, \db -> p (db NHM.* d))

  atan2 (Diff f) (Diff g) = Diff $ \s ->
    let (y, py) = f s
        (x, px) = g s
        r = NHF.atan2 y x
        denom = y NHM.* y NHA.+ x NHM.* x
        dy = x NHM./ denom
        dx = NHA.negate (y NHM./ denom)
     in (r, \db -> py (db NHM.* dy) NHA.+ px (db NHM.* dx))

  sinh (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (NHF.sinh b, \db -> p (db NHM.* NHF.cosh b))

  cosh (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (NHF.cosh b, \db -> p (db NHM.* NHF.sinh b))

  asinh (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = NHM.recip (NHF.sqrt (b NHM.* b NHA.+ NHM.one))
     in (NHF.asinh b, \db -> p (db NHM.* d))

  acosh (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = NHM.recip (NHF.sqrt (b NHM.* b NHA.- NHM.one))
     in (NHF.acosh b, \db -> p (db NHM.* d))

  atanh (Diff f) = Diff $ \s ->
    let (b, p) = f s
        d = NHM.recip (NHM.one NHA.- b NHM.* b)
     in (NHF.atanh b, \db -> p (db NHM.* d))

-- | Mechanical 'Num' mirror so that base-polymorphic code can also use 'Diff'
-- as its carrier.  Non-smooth methods ('abs', 'signum') raise an error; the
-- useful cases (literals, '+', '*', '-') work out of the box.
instance (P.Num s, P.Num b) => P.Num (Diff' p s b) where
  Diff f + Diff g = Diff $ \s ->
    let (b1, p1) = f s
        (b2, p2) = g s
     in (b1 P.+ b2, \db -> p1 db P.+ p2 db)

  Diff f * Diff g = Diff $ \s ->
    let (b1, p1) = f s
        (b2, p2) = g s
     in (b1 P.* b2, \db -> p1 (db P.* b2) P.+ p2 (b1 P.* db))

  negate (Diff f) = Diff $ \s ->
    let (b, p) = f s
     in (P.negate b, P.negate . p)

  Diff f - Diff g = Diff $ \s ->
    let (b1, p1) = f s
        (b2, p2) = g s
     in (b1 P.- b2, \db -> p1 db P.- p2 db)

  abs _ = P.error "NumHask.Diff: abs is not differentiable at 0"
  signum _ = P.error "NumHask.Diff: signum is not differentiable at 0"

  fromInteger n = Diff (const (P.fromInteger n, const 0))

