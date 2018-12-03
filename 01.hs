{-# LANGUAGE ScopedTypeVariables #-}

import Util

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

readInt :: String -> Int
readInt x = read $ if head x == '+' then tail x else x

firstDuplicate :: [Int] -> Int -> IntSet -> Int
firstDuplicate (x:xs) current seen = 
  if current `IntSet.member` seen
  then current
  else firstDuplicate xs (current + x) (IntSet.insert current seen)

main = do
  input <- getInput "01"
  let nums :: [Int] = map readInt $ input

  print $ sum nums -- *1
  print $ firstDuplicate (cycle nums) 0 IntSet.empty -- *2
