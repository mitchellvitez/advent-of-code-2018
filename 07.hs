{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Util

import Data.Attoparsec.Text
import Data.List
import Control.Lens
import Control.Monad.State

data WorkerData = WorkerData
  { _seconds :: Int
  , _timeRemaining :: [(Char, Int)]
  , _workingOn :: [Char]
  , _assoc :: [(Char, Char)]
  , _origAssoc :: [(Char, Char)]
  , _stored :: Char
  }
makeLenses ''WorkerData

main = do
  input <- parseInput parseSteps "07"

  putStrLn $ reduce input ' ' -- *1

  let initialTime = zip ['A'..'Z'] [61..]
  print $ evalState howLong (WorkerData 0 initialTime [] input input ' ')

reduce :: [(Char, Char)] -> Char -> [Char]
reduce [] stored = [stored]
reduce list _ =
  firstChar : reduce remaining (snd $ head list)
  where
    (from, to) = unzip list
    firstChar = head . sort $ filter (`notElem` to) from
    remaining = filter ((/=firstChar) . fst) list

howLong :: State WorkerData Int
howLong = do
  wd <- get
  -- if we're done working on everything, return how long it took
  if null . filter ((/=0) . snd) $ wd ^. timeRemaining
  then return $ wd ^. seconds
  else do
    updateSeconds
    updateWorkingOn
    updateTimeRemaining
    findNextJob
    howLong

-- for each in time remaining
-- subtract 1 second if it's in "workingOn"
updateTimeRemaining :: State WorkerData ()
updateTimeRemaining = do
  wd <- get
  let newtr = flip map (wd ^. timeRemaining) $ \(a, b) -> 
        if a `elem` wd ^. workingOn
        then (a, b-1) 
        else (a, b)
  put $ wd & timeRemaining .~ newtr

-- if any in workingOn are now completed, remove them from workingOn
updateWorkingOn :: State WorkerData ()
updateWorkingOn = do
    wd <- get
    let newwo = filter (\c -> (c, 0) `notElem` wd ^. timeRemaining) (wd ^. workingOn)
    put $ wd & workingOn .~ newwo

-- increment seconds
updateSeconds :: State WorkerData ()
updateSeconds = do
  wd <- get
  put $ wd & seconds %~ (+1)

-- if workingOn not full, find next available job to be working on
findNextJob :: State WorkerData ()
findNextJob = do
  wd <- get
  when (length (wd ^. workingOn) < 5) $ do
    if null $ wd ^. assoc
    then
      -- handle the very last job
      put $ wd & workingOn %~ ((wd ^. stored):)
    else 
      nextStep

nextStep :: State WorkerData ()
nextStep = do
  wd <- get
  let (from, to) = unzip $ wd ^. assoc
  let firstChar = head . sort $ filter (`notElem` to) from
  -- firstChar = 'C'
  put $ wd & stored .~ (snd . head $ wd ^. assoc)
  -- set stored = 'B'
  wd <- get
  -- TODO: get this part working inside State
  -- if isDone $ previousStep firstChar
  -- then do
  let remaining = filter ((/=firstChar) . fst) $ wd ^. assoc
  put $ wd & assoc .~ remaining
  wd <- get
  put $ wd & workingOn %~ (firstChar:)
  -- else return ()

parseSteps :: Parser (Char, Char)
parseSteps = do
  string "Step "
  a <- anyChar
  string " must be finished before step "
  b <- anyChar
  string " can begin.\n"
  return (a, b)
