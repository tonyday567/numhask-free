-- | Square matrices and the polymorphic four-for-one.
--
-- 'starMatrix' is the 2×2 block-recursive Kleene star.  Instantiating
-- the carrier at different semirings yields four classical algorithms:
--
-- * __Bool__ — Warshall's transitive closure.
-- * __MinPlus__ — Floyd–Warshall shortest paths.
-- * __'StarSemiring'__ — Kleene's state elimination (regexes from automata).
-- * __'FieldStar'__ — matrix inversion @'(I − A)⁻¹'@ via Schur complement.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NumHask.Free.Matrix
  ( Matrix (..),
    matPlus,
    matTimes,
    matVec,
    starMatrix,
    -- * Demo carriers
    Warshall (..),
    MinPlus (..),
    FieldStar (..),
  )
where

import NumHask.Algebra.Additive qualified as NHA
import NumHask.Algebra.Multiplicative qualified as NHM
import NumHask.Algebra.Ring qualified as NHR
import Prelude (Bool, Double, Eq, Fractional, Num, Ord, Show, all, foldr, length, take, drop, zip, zipWith, (++))
import Prelude qualified as P

-- $setup
-- >>> import qualified NumHask.Free.StarSemiring

-- | Square matrix stored row-major.
newtype Matrix a = Matrix { unMatrix :: [[a]] }
  deriving (Eq, Show)

-- | Elementwise addition.
matPlus :: NHA.Additive a => Matrix a -> Matrix a -> Matrix a
matPlus (Matrix a) (Matrix b) =
  Matrix [zipWith (NHA.+) rowA rowB | (rowA, rowB) <- zip a b]

-- | Matrix–vector product.
--
-- The solver's output side: @matVec ('starMatrix' a) v@ solves the
-- affine fixpoint @x = a·x + v@.
matVec :: (NHA.Additive a, NHM.Multiplicative a) => Matrix a -> [a] -> [a]
matVec (Matrix m) v =
  [foldr (NHA.+) NHA.zero (zipWith (NHM.*) row v) | row <- m]

-- | Matrix multiplication.
matTimes ::
  (NHA.Additive a, NHM.Multiplicative a) =>
  Matrix a ->
  Matrix a ->
  Matrix a
matTimes (Matrix a) (Matrix b) =
  Matrix [[foldr (NHA.+) NHA.zero (zipWith (NHM.*) row col) | col <- transpose b] | row <- a]
  where
    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose xss
      | all P.null xss = []
      | P.otherwise = [h | (h : _) <- xss] : transpose [t | (_ : t) <- xss]

-- | Partition a square matrix into four quadrants.
--
-- Assumes the matrix size is at least 1.
partition :: Matrix a -> (Matrix a, Matrix a, Matrix a, Matrix a)
partition (Matrix m) =
  let n = length m
      k = n `P.div` 2
      top = take k m
      bot = drop k m
      a = Matrix [take k row | row <- top]
      b = Matrix [drop k row | row <- top]
      c = Matrix [take k row | row <- bot]
      d = Matrix [drop k row | row <- bot]
   in (a, b, c, d)

-- | Combine four quadrants into a single matrix.
combine :: Matrix a -> Matrix a -> Matrix a -> Matrix a -> Matrix a
combine (Matrix a) (Matrix b) (Matrix c) (Matrix d) =
  Matrix ([rowA ++ rowB | (rowA, rowB) <- zip a b] ++
          [rowC ++ rowD | (rowC, rowD) <- zip c d])

-- | Kleene star of a square matrix by 2×2 block recursion.
--
-- The polymorphic eliminator: one function, four algorithms.
--
-- === Warshall (Bool) — transitive closure
--
-- >>> :{
-- let m = Matrix [[Warshall False, Warshall True],
--                 [Warshall False, Warshall False]]
-- :}
-- >>> starMatrix m
-- Matrix {unMatrix = [[Warshall True,Warshall True],[Warshall False,Warshall True]]}
--
-- === Floyd–Warshall (MinPlus) — shortest paths
--
-- Nonnegative weights only; a negative cycle would yield @-∞@.
--
-- >>> :{
-- let inf = MinPlus (1/0)
--     m = Matrix [[MinPlus 0, MinPlus 3, inf],
--                 [inf, MinPlus 0, MinPlus 1],
--                 [MinPlus 2, inf, MinPlus 0]]
-- :}
-- >>> starMatrix m
-- Matrix {unMatrix = [[MinPlus 0.0,MinPlus 3.0,MinPlus 4.0],[MinPlus 3.0,MinPlus 0.0,MinPlus 1.0],[MinPlus 2.0,MinPlus 5.0,MinPlus 0.0]]}
--
-- === State elimination (free 'StarSemiring') — regexes from automata
--
-- >>> :{
-- let s = NumHask.Free.StarSemiring.embed
--     m = Matrix [[s "a", s "b"], [s "c", s "d"]]
-- :}
-- >>> starMatrix m
-- Matrix {unMatrix = ...}
--
-- === Matrix inversion (FieldStar) — @(I − A)⁻¹@
--
-- Partial at @a = 1@; IEEE 754 returns 'Infinity', which satisfies
-- the star equation in the extended reals.
--
-- >>> :{
-- let m = Matrix [[FieldStar 0.1, FieldStar 0.2],
--                 [FieldStar 0.3, FieldStar 0.1]]
-- :}
-- >>> starMatrix m
-- Matrix {unMatrix = ...}
starMatrix :: NHR.StarSemiring a => Matrix a -> Matrix a
starMatrix (Matrix []) = Matrix []
starMatrix m =
  case unMatrix m of
    [[a]] -> Matrix [[NHR.star a]]
    _ ->
      let (a, b, c, d) = partition m
          dStar = starMatrix d
          f = matPlus a (matTimes b (matTimes dStar c))
          fStar = starMatrix f
          e = fStar
          fBlock = matTimes fStar (matTimes b dStar)
          g = matTimes dStar (matTimes c fStar)
          h = matPlus dStar (matTimes dStar (matTimes c (matTimes fStar (matTimes b dStar))))
       in combine e fBlock g h

-- ---------------------------------------------------------------------------
-- Demo carriers
-- ---------------------------------------------------------------------------

-- | Boolean semiring for Warshall's transitive closure.
--
-- 'plus' is '||', 'times' is '&&', 'star' is constantly 'True'.
newtype Warshall = Warshall Bool
  deriving (Eq, Ord, Show)

instance NHM.Multiplicative Warshall where
  one = Warshall P.True
  Warshall a * Warshall b = Warshall (a P.&& b)

instance NHA.Additive Warshall where
  zero = Warshall P.False
  Warshall a + Warshall b = Warshall (a P.|| b)

instance NHR.StarSemiring Warshall where
  star _ = Warshall P.True

-- | Tropical (min-plus) semiring for Floyd–Warshall shortest paths.
--
-- 'plus' is 'min', 'times' is '+', 'star' is constantly '0'.
newtype MinPlus = MinPlus Double
  deriving (Eq, Ord, Show)

instance NHM.Multiplicative MinPlus where
  one = MinPlus 0
  MinPlus a * MinPlus b = MinPlus (a P.+ b)

instance NHA.Additive MinPlus where
  zero = MinPlus (1 P./ 0)
  MinPlus a + MinPlus b = MinPlus (P.min a b)

instance NHR.StarSemiring MinPlus where
  star _ = NHM.one

-- | Field semiring for matrix inversion @(I − A)⁻¹@.
--
-- 'star' is the closed Neumann series: @star a = recip (1 − a)@.
newtype FieldStar = FieldStar {unFieldStar :: Double}
  deriving (Eq, Ord, Show, Num, Fractional)

instance NHM.Multiplicative FieldStar where
  one = FieldStar 1
  FieldStar a * FieldStar b = FieldStar (a P.* b)

instance NHA.Additive FieldStar where
  zero = FieldStar 0
  FieldStar a + FieldStar b = FieldStar (a P.+ b)

instance NHA.Subtractive FieldStar where
  negate (FieldStar a) = FieldStar (P.negate a)
  FieldStar a - FieldStar b = FieldStar (a P.- b)

instance NHR.StarSemiring FieldStar where
  star (FieldStar a) = FieldStar (P.recip (1 P.- a))
