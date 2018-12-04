{-# LANGUAGE TemplateHaskell #-}

import Util

import Data.List
import Data.Either (fromRight)
import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Number (decimal)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Ord
import Control.Lens

data Action = WakeUp | FallAsleep | BeginShift Int
  deriving (Eq, Ord, Show)

type GuardData = (Int, [Int])

data Entry = Entry
  { _minute :: Int
  , _action :: Action
  }
  deriving (Eq, Ord, Show)
makeLenses ''Entry

data GuardInfo = GuardInfo
  { _currentGuard :: Int
  , _asleepAt :: Int
  , _guardData :: IntMap GuardData
  }
makeLenses ''GuardInfo

main = do
  input <- Util.getInput "04"

  let entries = fromRight [] $ (parse (many parseInput) "") $ unlines $ sort input
  let defaultInfo = GuardInfo 0 0 IntMap.empty
  let minutes = IntMap.toList . view guardData $ foldl' countMinutes defaultInfo entries

  printAnswer (fst . snd) minutes -- *1
  printAnswer (mostCommonMinute . snd . snd) minutes -- *2

printAnswer comparison = print . toAnswer . maximumBy (comparing comparison)

toAnswer (guard, (_, list)) = (guard*) . head $ mostCommonMinute list

mostCommonMinute = maximumBy (comparing length) . group . sort

countMinutes :: GuardInfo -> Entry -> GuardInfo
countMinutes info entry =
  case entry ^. action of
    BeginShift newGuard ->
      info & currentGuard .~ newGuard

    FallAsleep ->
      info & asleepAt .~ entry ^. minute

    WakeUp ->
      info & guardData .~
        IntMap.insertWith addStuff (info ^. currentGuard) guardDatum (info ^. guardData) 
      where min = entry ^. minute
            asleep = info ^. asleepAt
            guardDatum = (min - asleep, [asleep..min-1])

addStuff :: GuardData -> GuardData -> GuardData
addStuff (m1, lst1) (m2, lst2) = (m1 + m2, lst1 ++ lst2)

parseInput :: Parser Entry
parseInput = do
  many (Parsec.noneOf ":")
  char ':'
  minute <- decimal
  string "] "
  info <- parseInfo
  char '\n'
  return $ Entry minute info

parseInfo :: Parser Action
parseInfo = do
      FallAsleep <$ string "falls asleep"
  <|> WakeUp <$ string "wakes up"
  <|> do 
      string "Guard #"
      guardId <- decimal
      string " begins shift"
      return $ BeginShift guardId
