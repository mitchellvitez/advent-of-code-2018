import Util

import Data.Attoparsec.Text

data Node = Node 
  { children :: [Node]
  , entries :: [Int]
  }
  deriving Show

main = do
  input <- parseInput parseTree "08"

  let root = head input
  print $ sumEntries root
  print $ getValue root

sumEntries n = sum (entries n) + (sum . map sumEntries $ children n)

getValue node =
  if null (children node)
  then sum (entries node)
  else sum . map (getValueIfExists node) $ entries node

getValueIfExists node index =
  if length (children node) >= index
  then getValue $ children node !! (index - 1)
  else 0

parseTree = do
  childNodes <- decimal
  space
  metadata <- decimal
  space
  subtrees <- count childNodes parseTree
  entries <- count metadata parseMetadata
  return $ Node subtrees entries

parseMetadata = do
  x <- decimal
  space
  return x
