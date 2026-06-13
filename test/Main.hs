module Main where

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
