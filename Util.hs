module Util where

import Data.List (tails)

pairs xs ys = [ (x, y) | (x:rest) <- tails xs, y <- rest ]

getInput s = do
  f <- readFile $ "input/" ++ s ++ ".txt"
  return $ lines f

howManyTrue = length . filter (==True)
