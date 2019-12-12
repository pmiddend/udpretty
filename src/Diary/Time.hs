{-# LANGUAGE OverloadedStrings #-}

module Diary.Time where

import Control.Lens (Getter, Iso', Lens', _2, _3, iso, to)
import Data.Text (Text)
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Calendar.WeekDate as CalWd

gregorian :: Iso' Cal.Day (Integer, Int, Int)
gregorian = iso Cal.toGregorian (\(a, b, c) -> Cal.fromGregorian a b c)

weekDate :: Iso' Cal.Day (Integer, Int, Int)
weekDate = iso CalWd.toWeekDate (\(a, b, c) -> CalWd.fromWeekDate a b c)

dayOfMonth :: Lens' Cal.Day Int
dayOfMonth = gregorian . _3

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Eq, Ord)

data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq, Ord)

monthEnum :: Iso' Int Month
monthEnum = iso intToMonth monthToInt
  where
    monthToInt January = 1
    monthToInt February = 2
    monthToInt March = 3
    monthToInt April = 4
    monthToInt May = 5
    monthToInt June = 6
    monthToInt July = 7
    monthToInt August = 8
    monthToInt September = 9
    monthToInt October = 10
    monthToInt November = 11
    monthToInt December = 12
    intToMonth i =
      case i of
        1 -> January
        2 -> February
        3 -> March
        4 -> April
        5 -> May
        6 -> June
        7 -> July
        8 -> August
        9 -> September
        10 -> October
        11 -> November
        12 -> December
        _ -> error "invalid month"

weekDayNumber :: Iso' Int WeekDay
weekDayNumber = iso intToWeekDay weekDayToInt
  where
    weekDayToInt Monday = 1
    weekDayToInt Tuesday = 2
    weekDayToInt Wednesday = 3
    weekDayToInt Thursday = 4
    weekDayToInt Friday = 5
    weekDayToInt Saturday = 6
    weekDayToInt Sunday = 7
    intToWeekDay i =
      case i of
        1 -> Monday
        2 -> Tuesday
        3 -> Wednesday
        4 -> Thursday
        5 -> Friday
        6 -> Saturday
        7 -> Sunday
        _ -> error "invalid weekday"

monthForDay :: Lens' Cal.Day Month
monthForDay = gregorian . _2 . monthEnum

weekDayForDay :: Lens' Cal.Day WeekDay
weekDayForDay = weekDate . _3 . weekDayNumber

dayOfWeekStr :: Getter WeekDay Text
dayOfWeekStr = to dayOfWeekToStr
  where
    dayOfWeekToStr Monday = "Montag"
    dayOfWeekToStr Tuesday = "Dienstag"
    dayOfWeekToStr Wednesday = "Mittwoch"
    dayOfWeekToStr Thursday = "Donnerstag"
    dayOfWeekToStr Friday = "Freitag"
    dayOfWeekToStr Saturday = "Samstag"
    dayOfWeekToStr Sunday = "Sonntag"

monthStr :: Getter Month Text
monthStr = to monthToStr
  where
    monthToStr January = "Januar"
    monthToStr February = "Februar"
    monthToStr March = "März"
    monthToStr April = "April"
    monthToStr May = "Mai"
    monthToStr June = "Juni"
    monthToStr July = "Juli"
    monthToStr August = "August"
    monthToStr September = "September"
    monthToStr October = "Oktober"
    monthToStr November = "November"
    monthToStr December = "Dezember"
