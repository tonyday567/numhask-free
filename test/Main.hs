module Main where

import NumHask.Free.Matrix
import NumHask.Free.StarSemiring
import System.Exit (exitFailure)
import Prelude

assert :: String -> Bool -> IO ()
assert msg ok = do
  if ok then putStrLn ("✓ " ++ msg) else do
    putStrLn ("✗ " ++ msg)
    exitFailure

main :: IO ()
main = do
  assert "kleeneSimplify collapses duplicates" $
    let t1 = plus (embed "x") (plus (embed "x") (embed "y"))
     in kleeneSimplify t1 == plus (embed "x") (embed "y")

  assert "kleeneSimplify recurses under Times" $
    let t2 = times (plus (embed "x") (embed "x")) (embed "y")
     in kleeneSimplify t2 == times (embed "x") (embed "y")

  assert "star zero = one" $
    (star zero :: StarSemiring String) == one

  assert "Warshall transitive closure" $
    let m = Matrix [[Warshall False, Warshall True], [Warshall False, Warshall False]]
     in starMatrix m == Matrix [[Warshall True, Warshall True], [Warshall False, Warshall True]]

  assert "Floyd–Warshall shortest paths" $
    let inf = MinPlus (1 / 0)
        m = Matrix [[MinPlus 0, MinPlus 3, inf], [inf, MinPlus 0, MinPlus 1], [MinPlus 2, inf, MinPlus 0]]
        expected = Matrix [[MinPlus 0, MinPlus 3, MinPlus 4], [MinPlus 3, MinPlus 0, MinPlus 1], [MinPlus 2, MinPlus 5, MinPlus 0]]
     in starMatrix m == expected

  assert "Field star matrix inversion" $
    let m = Matrix [[FieldStar 0.1, FieldStar 0.2], [FieldStar 0.3, FieldStar 0.1]]
        rows = map (map (\(FieldStar x) -> x)) (unMatrix (starMatrix m))
     in case rows of
          [[a, b], [c, d]] ->
            abs (a - 1.2) < 1e-10 && abs (b - 0.2666666666666667) < 1e-10 &&
            abs (c - 0.4) < 1e-10 && abs (d - 1.2) < 1e-10
          _ -> False

  assert "starMatrix empty matrix" $
    let m = Matrix [] :: Matrix Warshall
     in starMatrix m == m
