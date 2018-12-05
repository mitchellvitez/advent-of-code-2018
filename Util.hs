module Util where

import Data.List (tails)
import qualified Data.Text.IO as Text
import Data.Text (Text)
import Data.Either (fromRight)
import Data.Attoparsec.Text
import Data.Text (Text)

getInput day = do
  f <- readFile $ "input/" ++ day ++ ".txt"
  return $ lines f

parseInput parser day = do
  input <- Text.readFile $ "input/" ++ day ++ ".txt"
  return . fromRight [] $ parseOnly (many' parser) input
