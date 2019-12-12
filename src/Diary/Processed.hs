{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Diary.Processed where

import Control.Lens
  ( Fold
  , (^.)
  , (^..)
  , (^?!)
  , _Just
  , filtered
  , firstOf
  , foldMapOf
  , from
  , has
  , lengthOf
  , makeLenses
  , maximumOf
  , minimumOf
  , only
  , sumOf
  , to
  , view
  )
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max(..))
import Data.Text (Text, filter, isPrefixOf, toLower)
import Data.Text.Lens (packed)
import Data.Time.Calendar (Day)
import Debug.Trace (trace)
import Diary.Json
  ( DiaryMark
  , DiaryShape(StarShape)
  , dmComment
  , dmDate
  , dmGrade
  , dmPhoto
  , dmShape
  )
import Diary.Time (Month)
import Diary.Utility (groupByPairs, worded)
import Prelude hiding (filter, unwords, words)

data ProcessedPhoto =
  ProcessedPhoto
    { _ppFile :: FilePath
    , _ppCaption :: Maybe Text
    }
  deriving (Show, Eq)

makeLenses ''ProcessedPhoto

data ProcessedDay =
  ProcessedDay
    { _pdDay :: Day
    , _pdGrade :: Int
    , _pdStar :: Bool
    , _pdComments :: [Text]
    , _pdPhotos :: [ProcessedPhoto]
    , _pdProjectsStarted :: [Text]
    , _pdProjectsFinished :: [Text]
    , _pdSpecials :: [Text]
    }
  deriving (Show, Eq)

makeLenses ''ProcessedDay

data ProcessedMonth =
  ProcessedMonth
    { _pmMonth :: Month
    , _pmDays :: [ProcessedDay]
    , _pmAvgGrade :: Double
    , _pmProjectsStarted :: [Text]
    , _pmProjectsFinished :: [Text]
    , _pmSpecials :: [Text]
    }
  deriving (Show, Eq)

makeLenses ''ProcessedMonth

data ProcessedYear =
  ProcessedYear
    { _pyAvgGrade :: Double
    , _pyMonths :: [ProcessedMonth]
    , _pyMinAvg :: Double
    , _pyMaxAvg :: Double
    }
  deriving (Show, Eq)

makeLenses ''ProcessedYear

mkProcessedYear :: [ProcessedMonth] -> ProcessedYear
mkProcessedYear months =
  ProcessedYear
    { _pyAvgGrade =
        fromIntegral (sumOf (traverse . pmDays . traverse . pdGrade) months) /
        fromIntegral (lengthOf (traverse . pmDays . traverse) months)
    , _pyMonths = months
    , _pyMinAvg = (fromMaybe 0 (minimumOf (traverse . pmAvgGrade) months))
    , _pyMaxAvg = (fromMaybe 0 (maximumOf (traverse . pmAvgGrade) months))
    }

filterNotInClass :: String -> Text -> Text
filterNotInClass s = filter (not . (`elem` s))

tagsForComment :: Fold Text Text
tagsForComment =
  worded . filtered ("#" `isPrefixOf`) . to (filterNotInClass ",!.") .
  to toLower

projectStartForComment :: Text -> Maybe Text
projectStartForComment t
  | "anfang " `isPrefixOf` toLower t = firstOf tagsForComment t
  | otherwise = Nothing

projectEndForComment :: Text -> Maybe Text
projectEndForComment t
  | "ende " `isPrefixOf` toLower t = firstOf tagsForComment t
  | otherwise = Nothing

processDay :: (Day, NonEmpty DiaryMark) -> ProcessedDay
processDay (d, marks) =
  ProcessedDay
    { _pdDay = d
    , _pdGrade = getMax (foldMapOf (traverse . dmGrade . traverse) Max marks)
    , _pdStar = has (traverse . dmShape . only (Just StarShape)) marks
    , _pdComments = marks ^.. traverse . dmComment . traverse
    , _pdPhotos =
        marks ^.. traverse . filtered (has (dmPhoto . _Just)) .
        to
          (\x ->
             ProcessedPhoto
               (x ^?! dmPhoto . _Just . from packed)
               (x ^. dmComment))
    , _pdProjectsStarted =
        marks ^.. traverse . dmComment . traverse . to projectStartForComment .
        traverse
    , _pdProjectsFinished =
        marks ^.. traverse . dmComment . traverse . to projectEndForComment .
        traverse
    , _pdSpecials =
        marks ^.. traverse . dmComment . traverse .
        filtered (has (tagsForComment . only "#special"))
    }

processMonth :: (Month, NonEmpty DiaryMark) -> ProcessedMonth
processMonth (m, marks) =
  let daysRaw :: [(Day, NonEmpty DiaryMark)]
      daysRaw = groupByPairs (view dmDate) marks
      processedDays :: [ProcessedDay]
      processedDays = sortOn (view pdDay) (processDay <$> daysRaw)
   in ProcessedMonth
        { _pmMonth = m
        , _pmDays = processedDays
        , _pmAvgGrade =
            fromIntegral (sumOf (traverse . pdGrade) processedDays) /
            fromIntegral (length processedDays)
        , _pmProjectsStarted =
            processedDays ^.. traverse . pdProjectsStarted . traverse
        , _pmProjectsFinished =
            processedDays ^.. traverse . pdProjectsFinished . traverse
        , _pmSpecials = processedDays ^.. traverse . pdSpecials . traverse
        }
