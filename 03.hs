{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

import Util
import Data.Attoparsec.Text
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
-- import Data.Judy as J

data Claim = Claim
  { claimId :: Int
  , fromLeft :: Int
  , fromTop :: Int
  , width :: Int
  , height :: Int
  }
  deriving Show

main = do
  claims <- Util.parseInput parseClaim "03"

  let grid = IntMap.filter (>1) $ fillup IntMap.empty claims
  let overlapped = IntSet.fromList $ IntMap.keys grid

  print $ IntMap.size grid -- *1
  print . claimId . head $ filter (noOverlap overlapped) claims -- *2

  -- mutableVersion claims

noOverlap :: IntSet -> Claim -> Bool
noOverlap overlapped claim =
  Prelude.null . filter (`IntSet.member` overlapped) $ getIndices claim

getIndices :: Claim -> [Int]
getIndices c = do
  x <- [fromLeft c + 1 .. fromLeft c + width c]
  y <- [fromTop c + 1 .. fromTop c + height c]
  -- simple hash function so we can use IntMap Int instead of Map (Int, Int) Int
  return $ 10000 * x + y

fillup :: IntMap Int -> [Claim] -> IntMap Int
fillup grid claims =
  foldl' claimAll grid claims
  where
    claimAll grid claim =
      IntMap.unionWith (+) grid . IntMap.fromList $ (,1) <$> getIndices claim

parseClaim :: Parser Claim
parseClaim = do
  char '#'
  claimId <- decimal <* string " @ "
  fromLeft <- decimal <* char ','
  fromTop <- decimal <* string ": "
  width <- decimal <* char 'x'
  height <- decimal <* char '\n'
  return $ Claim claimId fromLeft fromTop width height


-- -- This version uses mutable data, but it turned out to be slower
--
-- mutableVersion claims = do
--   mutGrid <- J.new :: IO (J.JudyL Int)

--   fillupJ mutGrid claims
--   grid <- J.freeze mutGrid
--   g <- toList grid
--   let g2 = filter (\(a,b) -> b > 1) g
--   let (keys, values) = unzip g2
--   let overlapped = IntSet.fromList $ map fromIntegral keys

--   print . length . filter (>1) $ values
--   print . claimId . head $ filter (noOverlap overlapped) claims

-- fillupJ :: J.JudyL Int -> [Claim] -> IO ()
-- fillupJ grid claims =
--   mapM_ (claimAll grid) claims

-- claimAll :: J.JudyL Int -> Claim -> IO ()
-- claimAll grid claim =
--   mapM_ (claimOne grid) (map fromIntegral (getIndices claim))

-- claimOne :: J.JudyL Int -> Int -> IO ()
-- claimOne grid k = do
--   J.insertWith (+) (fromIntegral k) 1 grid
