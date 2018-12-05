import Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Util
import Control.Monad.Cont
import System.Exit

has n = length . filter (elem n . map length . group . sort)

diffByOne xs ys = (==1) . length . filter (==True) $ zipWith (/=) xs ys

sameChars xs ys = map fst . filter (uncurry (==)) $ zip xs ys

pairs xs ys = [ (x, y) | (x:rest) <- tails xs, y <- rest ]

main = do
  input <- getInput "02"
  print $ has 2 input * has 3 input -- *1

  -- This is a slower-running way to do part 2 (original solution)
  -- putStrLn . uncurry sameChars . head . filter (uncurry diffByOne) $ pairs input input

  let len = length $ head input
  mapM_ (check Set.empty input) [0..len]

check :: Set String -> [String] -> Int -> IO ()
check _ [] _ = return ()
check seen (x:xs) i = do
  let (a, b) = List.splitAt i x
  let spliced = a ++ tail b
  when (spliced `Set.member` seen) $ do
    liftIO $ putStrLn spliced -- *2
    exitSuccess
  check (Set.insert spliced seen) xs i
