{-# LANGUAGE OverloadedStrings #-}

import Util

import Data.Attoparsec.Text
import Data.Matrix as Matrix
import Data.List
import Data.Ord
import qualified Data.Vector as Vector
import qualified Data.Set as Set
import Data.Monoid ((<>))

main = do
  input <- parseInput parseCoord "06"

  let (xs, ys) = unzip input
  let newxs = map (subtract (minimum xs)) xs
  let newys = map (subtract (minimum ys)) ys
  let size = max (maximum newxs) (maximum newys)

  result <- buildGrid size $ zip newxs newys
  print result -- *1

  let regionGrid = matrix size size (inRegion 10000 input)
  print . length . filter (=='#') $ Matrix.toList regionGrid -- *2

buildGrid size input = do
  let grid = matrix size size (setGrid input)
  let infinite = Set.fromList . Vector.toList $
        getRow 1 grid <> getRow size grid <> getCol 1 grid <> getCol size grid
  return . last . sort . map length . group . sort .
    filter (`Set.notMember` infinite) $ Matrix.toList grid

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a, b) (c, d) =
  abs (a - c) + abs (b - d)

inRegion dist input coord = 
  if (<dist) . sum $ map (manhattan coord) input then '#' else '.'

setGrid input coord =
  -- tried using kd trees but it was slower on this dataset
  let (a:b:_) = sortBy (comparing (manhattan coord)) input
    in if manhattan coord a == manhattan coord b then (1, 1) else a

parseCoord :: Parser (Int, Int)
parseCoord = do
  x <- decimal
  string ", "
  y <- decimal
  char '\n'
  return $ (x, y)
