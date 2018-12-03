import Util
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (decimal)
import Data.List
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Grid = Map (Int, Int) Int

data Claim = Claim
  { claimId :: Int
  , fromLeft :: Int
  , fromTop :: Int
  , width :: Int
  , height :: Int
  }
  deriving Show

main = do
  input <- Util.getInput "03"

  let claims = fromRight [] . parse (many parseClaim) "" $ unlines input
  let grid = Map.filter (>1) $ fillup Map.empty claims
  let overlapped = Set.fromList $ Map.keys grid

  print $ Map.size grid -- *1
  print . claimId . head $ filter (noOverlap overlapped) claims -- *2

noOverlap :: Set (Int, Int) -> Claim -> Bool
noOverlap overlapped claim =
  null . filter (`Set.member` overlapped) $ getIndices claim

getIndices :: Claim -> [(Int, Int)]
getIndices c = do
  x <- [fromLeft c + 1 .. fromLeft c + width c]
  y <- [fromTop c + 1 .. fromTop c + height c]
  return (x, y)

fillup :: Grid -> [Claim] -> Grid
fillup grid claims =
  let claimOne grid k = Map.insertWith (+) k 1 grid
      claimAll grid claim = foldl' claimOne grid (getIndices claim)
  in foldl' claimAll grid claims

parseClaim :: Parser Claim
parseClaim = do
  char '#'
  claimId <- decimal
  string " @ "
  fromLeft <- decimal
  char ','
  fromTop <- decimal
  string ": "
  width <- decimal
  char 'x'
  height <- decimal
  char '\n'
  return $ Claim claimId fromLeft fromTop width height
