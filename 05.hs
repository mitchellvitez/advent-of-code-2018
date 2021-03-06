import Util

import Data.Char
import Data.Bits

main = do
  input <- getInput "05"

  let polymer = head input
  let prereduced = reduce polymer

  print $ length prereduced -- *1
  print . minimum $ map (createReduction prereduced) $ map ord ['a'..'z'] -- *2

createReduction polymer c =
  length . reduce $ filter (caseInsensNotEqual c) polymer
  where caseInsensNotEqual a b = a /= ord b && a - 32 /= ord b

reduce [] = []
reduce (x:unreducedxs)
  | xs == [] = [x]
  | ord x `xor` (ord $ head xs) == 32 = tail xs
  | otherwise = x : xs
  where xs = reduce unreducedxs
