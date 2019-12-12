{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
  ( (^.)
  , (^..)
  , _2
  , _Show
  , filtered
  , from
  , lengthOf
  , only
  , re
  , sumOf
  , to
  , to
  , view
  )
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy (getContents)
import qualified Data.Csv as Csv
import Data.Foldable (elem, fold, toList)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.Ratio (Ratio, (%))
import Data.Set (Set, fromList)
import Data.Text (Text, breakOn, intercalate)
import Data.Text.IO as TextIO
import Data.Text.Lens (packed)
import Data.Time.Calendar (fromGregorian)
import Diary.Json (DiaryMark, ddMarks, dmDate, jsonDecode)
import Diary.Processed
  ( ProcessedDay
  , ProcessedMonth
  , ProcessedYear
  , mkProcessedYear
  , pdComments
  , pdDay
  , pdGrade
  , pdPhotos
  , pdStar
  , pmAvgGrade
  , pmDays
  , pmMonth
  , pmProjectsFinished
  , pmProjectsStarted
  , pmSpecials
  , ppCaption
  , ppFile
  , processMonth
  , pyMaxAvg
  , pyMinAvg
  , pyMonths
  , tagsForComment
  )
import Diary.Tex
  ( captionSetupNoLabel
  , declareUnicodeCharacter
  , figure
  , heart
  , protected
  , realNewline
  , sansSerifFont
  , subcaption
  , subfigure
  , textcomp
  , textdegree
  , texteuro
  )
import Diary.Time
  ( Month
  , dayOfMonth
  , dayOfWeekStr
  , monthEnum
  , monthForDay
  , monthStr
  , weekDayForDay
  )
import Diary.Utility (between, groupByPairs)
import Prelude hiding (getContents, head, putStr, putStr, putStrLn)
import qualified Text.LaTeX as Tex
import Text.LaTeX.Base.Commands (ClassOption(Paper), PaperType(A4))
import Text.LaTeX.Base.Syntax (Measure(Cm), Measure(CustomMeasure))
import Text.LaTeX.Packages.Graphicx (IGOption(..), graphicx, includegraphics)
import Text.LaTeX.Packages.Inputenc (inputenc, utf8)
import Text.Printf (printf)

makeTexDiary :: Monad m => Tex.LaTeXT_ m -> Tex.LaTeXT_ m
makeTexDiary content = do
  Tex.documentclass [Paper A4] Tex.article
  Tex.usepackage [utf8] inputenc
  Tex.usepackage [] graphicx
  Tex.usepackage [] textcomp
  Tex.usepackage [] subcaption
  Tex.usepackage [] "float"
  Tex.usepackage [] "caption"
  Tex.usepackage [] "titling"
  Tex.usepackage [] "pgf-pie"
  sansSerifFont
  -- heart
  declareUnicodeCharacter "2764" heart
  -- another heart
  declareUnicodeCharacter "2665" heart
  -- "selector", probably for color of the heart?
  declareUnicodeCharacter "FE0F" heart
  -- sushi, actually
  declareUnicodeCharacter "1F363" heart
  -- euro symbol
  declareUnicodeCharacter "20AC" texteuro
  -- degrees
  declareUnicodeCharacter "B0" textdegree
  Tex.title "The year"
  Tex.author "Some author"
  Tex.document $ do
    Tex.raw "\\begin{titlingpage}"
    Tex.raw "\\begin{center}"
    Tex.raw "\\section*{The year}"
    Tex.raw
      "\\begin{figure}[H]\\centering\\includegraphics[width=14cm]{wordcloud}\\end{figure}"
    Tex.raw "\\end{center}"
    Tex.raw "\\end{titlingpage}"
    content

normalizedMood :: ProcessedYear -> ProcessedMonth -> Text
normalizedMood py pm =
  let yearMin = py ^. pyMinAvg
      yearMax = py ^. pyMaxAvg
      result = ((pm ^. pmAvgGrade) - yearMin) / (yearMax - yearMin) * 100.0
   in (printf "%.2f%%" result) ^. packed

makeTexMonth :: Monad m => ProcessedYear -> ProcessedMonth -> Tex.LaTeXT_ m
makeTexMonth _ pm = do
  Tex.newpage
  Tex.section' (pm ^. pmMonth . monthStr . to Tex.raw)
  Tex.subsection' "Stats"
  dayStats (pm ^.. pmDays . traverse)
  Tex.newpage
  -- Tex.description $ do
  --   Tex.item (Just "Stimmung")
  --   protected (normalizedMood py pm)
  Tex.subsection' "Specials"
  surroundFold
    (pm ^. pmSpecials)
    Tex.itemize
    ((Tex.item Nothing <>) . makeCommentMarkup)
  surroundFold
    (pm ^. pmProjectsStarted)
    ((Tex.subsection' "Angefangen" <>) . Tex.itemize)
    ((Tex.item Nothing <>) . makeCommentMarkup)
  surroundFold
    (pm ^. pmProjectsFinished)
    ((Tex.subsection' "Beendet" <>) . Tex.itemize)
    ((Tex.item Nothing <>) . makeCommentMarkup)
  Tex.newpage
  mapM_ makeTexDay (pm ^. pmDays)

surroundFold :: (Monoid p, Monoid t) => [a] -> (t -> p) -> (a -> t) -> p
surroundFold xs g f =
  case xs of
    [] -> mempty
    _ -> g (foldMap f xs)

makeCommentMarkup :: Monad m => Text -> Tex.LaTeXT_ m
makeCommentMarkup "" = mempty
makeCommentMarkup t =
  let (beforeHash, afterHash) = breakOn "#" t
   in protected beforeHash <>
      case afterHash of
        "" -> mempty
        _ ->
          let (hashTag, afterSpace) = breakOn " " afterHash
           in Tex.textbf (protected hashTag) <> makeCommentMarkup afterSpace

starDays :: [ProcessedMonth] -> Ratio Int
starDays m =
  let stars = lengthOf (traverse . pmDays . traverse . filtered (view pdStar)) m
      total = lengthOf (traverse . pmDays . traverse) m
   in stars % total

makeTexDay :: Monad m => ProcessedDay -> Tex.LaTeXT_ m
makeTexDay pd =
  let dow :: Text
      dow = pd ^. pdDay . weekDayForDay . dayOfWeekStr
      dom :: Text
      dom = pd ^. pdDay . dayOfMonth . re _Show . packed
      mon :: Text
      mon = pd ^. pdDay . monthForDay . from monthEnum . re _Show . packed
      title = dow <> ", " <> dom <> "." <> mon <> "."
      includeGraphics photo =
        let caption =
              fold (Tex.caption . makeCommentMarkup <$> (photo ^. ppCaption))
            graphics =
              includegraphics
                [IGWidth (Cm 5.0), KeepAspectRatio True]
                (photo ^. ppFile)
         in Tex.centering <> graphics <> captionSetupNoLabel <> caption
      photos :: Monad m => Tex.LaTeXT_ m
      photos =
        surroundFold
          (pd ^. pdPhotos)
          figure
          (\photo ->
             subfigure
               (CustomMeasure (".5" <> Tex.textwidth))
               (includeGraphics photo) <>
             realNewline)
      marks :: Monad m => Tex.LaTeXT_ m
      marks =
        surroundFold
          (pd ^. pdComments)
          Tex.itemize
          (\comment -> Tex.item Nothing <> makeCommentMarkup comment)
   in Tex.subsection' (Tex.raw title) <> photos <> marks

pieChart :: Monad m => Text -> [(Text, Int)] -> Tex.LaTeXT_ m
pieChart caption values =
  let showValue (description, value) =
        (value ^. re _Show . packed) <> "/" <> description
      totalValue :: Int
      totalValue = sumOf (traverse . _2) values
      relativize :: Int -> Int
      relativize v =
        round (fromIntegral v / fromIntegral totalValue * 100.0 :: Float)
      relativeValues = second relativize <$> values
   in Tex.raw "\\begin{figure}[H]\\centering\\begin{tikzpicture}\\pie{" <>
      Tex.raw (intercalate "," (showValue <$> relativeValues)) <>
      Tex.raw ("}\\end{tikzpicture}\\caption{" <> caption <> "}\\end{figure}")

dayStats :: Monad m => [ProcessedDay] -> Tex.LaTeXT_ m
dayStats days = do
  let daysTotal :: Int
      daysTotal = lengthOf traverse days
      daysStarTotal :: Int
      daysStarTotal = lengthOf (traverse . pdStar . only True) days
      daysWithRating :: Int -> Int
      daysWithRating x = lengthOf (traverse . pdGrade . only x) days
  pieChart
    "Stars"
    [("Stars", daysStarTotal), ("No Stars", daysTotal - daysStarTotal)]
  pieChart
    "Good days, bad days"
    [ ("Excellent day", daysWithRating 3)
    , ("Good day", daysWithRating 2)
    , ("Bad day", daysWithRating 1)
    ]

main :: IO ()
main = do
  stdin <- BSL.getContents
  case jsonDecode stdin of
    Left e -> error e
    Right diary -> do
      let marks :: [DiaryMark]
          marks = diary ^. ddMarks
          yearStart = fromGregorian 2019 1 1
          yearEnd = fromGregorian 2019 12 31
          thisYearMarks = filter (between yearStart yearEnd . view dmDate) marks
          byMonths :: [(Month, NonEmpty DiaryMark)]
          byMonths =
            sortOn
              fst
              (groupByPairs (view (dmDate . monthForDay)) thisYearMarks)
          processedMonths :: [ProcessedMonth]
          processedMonths = processMonth <$> byMonths
          processedYear :: ProcessedYear
          processedYear = mkProcessedYear processedMonths
          texMonths :: Monad m => Tex.LaTeXT_ m
          texMonths =
            foldMap (makeTexMonth processedYear) (processedYear ^. pyMonths)
          allTags :: Set Text
          allTags =
            fromList
              (processedMonths ^.. traverse . pmDays . traverse . pdComments .
               traverse .
               tagsForComment)
          stats =
            Tex.newpage <> Tex.section' "Statistics" <>
            dayStats (processedMonths ^.. traverse . pmDays . traverse)
          content = stats <> texMonths
          makeDayLine :: ProcessedDay -> [Text]
          makeDayLine d =
            let dayStr = d ^. pdDay . re _Show . packed
                dayTags =
                  fromList (d ^.. pdComments . traverse . tagsForComment)
                dayTagBools =
                  (\tag ->
                     if tag `elem` dayTags
                       then "1"
                       else "0") <$>
                  toList allTags
             in [dayStr] <> dayTagBools
          dayHeaderLine :: [Text]
          dayHeaderLine = "Tag" : toList allTags
          dayLines :: [[Text]]
          dayLines =
            dayHeaderLine :
            (processedMonths ^.. traverse . pmDays . traverse . to makeDayLine)
      --BSL.putStr (Csv.encode dayLines)
      combined <- Tex.execLaTeXT (makeTexDiary content)
      TextIO.putStr (Tex.render combined)
