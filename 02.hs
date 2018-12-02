import Data.List
import Util

has n = length . filter (exactly n)

exactly n xs = n `elem` lengths xs

lengths = map length . group . sort

diffByOne = uncurry $ differentByOne 0

differentByOne :: Int -> String -> String -> Bool
differentByOne 1 [] [] = True
differentByOne i _ _
  | i > 1 = False
differentByOne i (x:xs) (y:ys)=
  if x /= y
    then differentByOne (i+1) xs ys
    else differentByOne i xs ys
differentByOne _ _ _ = False

sameChars [] [] = ""
sameChars (x:xs) (y:ys) =
  (if x == y then (x:) else id) $ sameChars xs ys

main = do
  input <- getInput "02"
  
  print $ has 2 input * has 3 input -- *1

  putStrLn . uncurry sameChars . head .
    filter diffByOne $ pairs input input -- *2
