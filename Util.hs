module Util where

pairs xs ys = [(x, y) | x <- xs, y <- ys]

pairsExcludeSame xs ys = [(x, y) | x <- xs, y <- ys, x /= y]

getInput s = do
  f <- readFile $ "input/" ++ s ++ ".txt"
  return $ lines f
