{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Diary.Json where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson
  ( FromJSON(..)
  , Value(String)
  , (.:)
  , (.:?)
  , eitherDecode
  , withObject
  )
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)

data DiaryMarkType
  = PhotoMark
  | RateMark
  | CommentMark
  | PaintMark
  | IdeaMark
  | MoneyMark
  | ShapeMark
  deriving (Show, Eq)

makePrisms ''DiaryMarkType

instance FromJSON DiaryMarkType where
  parseJSON (String "ru.schustovd.diary.api.PhotoMark") = pure PhotoMark
  parseJSON (String "ru.schustovd.diary.api.RateMark") = pure RateMark
  parseJSON (String "ru.schustovd.diary.api.CommentMark") = pure CommentMark
  parseJSON (String "ru.schustovd.diary.api.PaintMark") = pure PaintMark
  parseJSON (String "ru.schustovd.diary.api.IdeaMark") = pure IdeaMark
  parseJSON (String "ru.schustovd.diary.api.MoneyMark") = pure MoneyMark
  parseJSON (String "ru.schustovd.diary.api.ShapeMark") = pure ShapeMark
  parseJSON (String x) =
    fail ("parsing type failed, invalid string \"" <> show x <> "\"")
  parseJSON invalid =
    prependFailure "parsing type failed, " (typeMismatch "String" invalid)

data DiaryShape
  = RhombusShape
  | StarShape
  deriving (Show, Eq)

instance FromJSON DiaryShape where
  parseJSON (String "STAR") = pure StarShape
  parseJSON (String "RHOMBUS") = pure RhombusShape
  parseJSON (String x) =
    fail ("parsing shape failed, invalid string \"" <> show x <> "\"")
  parseJSON invalid =
    prependFailure "parsing shape failed, " (typeMismatch "String" invalid)

data DiaryMark =
  DiaryMark
    { _dmPhoto :: Maybe Text
    , _dmComment :: Maybe Text
    , _dmDate :: Day
    , _dmId :: Int
    , _dmTime :: TimeOfDay
    , _dmGrade :: Maybe Int
    , _dmShape :: Maybe DiaryShape
    , _dmType :: DiaryMarkType
    , _dmMoney :: Maybe Double
    }
  deriving (Show, Eq)

makeLenses ''DiaryMark

instance FromJSON DiaryMark where
  parseJSON =
    withObject "Mark" $ \v ->
      DiaryMark <$> (v .:? "photo") <*> (v .:? "comment") <*> (v .: "date") <*>
      (v .: "id") <*>
      (v .: "time") <*>
      (v .:? "grade") <*>
      (v .:? "shape") <*>
      (v .: "type") <*>
      (v .:? "money")

data DiaryDocument =
  DiaryDocument
    { _ddVersion :: Text
    , _ddMarks :: [DiaryMark]
    }
  deriving (Show, Eq)

instance FromJSON DiaryDocument where
  parseJSON =
    withObject "Document" $ \v ->
      DiaryDocument <$> (v .: "version") <*> (v .: "marks")

makeLenses ''DiaryDocument

jsonDecode :: ByteString -> Either String DiaryDocument
jsonDecode = eitherDecode
