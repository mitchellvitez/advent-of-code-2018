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
import Data.Biapplicative

type GuardId = Int
type Minute = Int

data Action = WakeUp | FallAsleep | BeginShift GuardId
  deriving (Eq, Ord, Show)

type GuardData = (Minute, [Minute])

data Entry = Entry
  { _key :: Int
  , _minute :: Minute
  , _action :: Action
  }
  deriving (Eq, Ord, Show)
makeLenses ''Entry

data GuardInfo = GuardInfo
  { _currentGuard :: GuardId
  , _asleepAt :: Minute
  , _guardData :: IntMap GuardData
  }
makeLenses ''GuardInfo

main = do
  input <- Util.getParseInput "04"

  let entries = sort . fromRight [] . (parse (many parseInput) "") $ input
  let defaultInfo = GuardInfo 0 0 IntMap.empty
  let minutes = IntMap.toList . view guardData $ foldl' countMinutes defaultInfo entries

  printAnswer (fst . snd) minutes -- *1
  printAnswer (mostCommonMinute . snd . snd) minutes -- *2

printAnswer comparison = print . toAnswer . maximumBy (comparing comparison)

toAnswer (guard, (_, minutes)) = (guard*) . head $ mostCommonMinute minutes

mostCommonMinute = maximumBy (comparing length) . group . sort

countMinutes :: GuardInfo -> Entry -> GuardInfo
countMinutes info entry =
  case entry ^. action of
    BeginShift newGuard ->
      info & currentGuard .~ newGuard

    FallAsleep ->
      info & asleepAt .~ entry ^. minute

    WakeUp ->
      info & guardData .~ IntMap.insertWith
        (biliftA2 (+) (++))
        (info ^. currentGuard)
        (min - asleep, [asleep..min-1])
        (info ^. guardData)
      where
        min = entry ^. minute
        asleep = info ^. asleepAt

parseInput :: Parser Entry
parseInput = do
  char '['
  -- parse date to Int because sorting by comparing strings is slow
  y <- decimal <* char '-'
  m <- decimal <* char '-'
  d <- decimal <* char ' '
  h <- decimal <* char ':'
  minute <- decimal <* string "] "
  info <- parseInfo <* char '\n'
  return $ Entry (1000000*y + 10000*m + 100*d + h) minute info

parseInfo :: Parser Action
parseInfo = do
      FallAsleep <$ string "falls asleep"
  <|> WakeUp <$ string "wakes up"
  <|> do 
      string "Guard #"
      guardId <- decimal
      string " begins shift"
      return $ BeginShift guardId
