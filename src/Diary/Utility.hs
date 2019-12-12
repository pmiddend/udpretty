module Diary.Utility where

import Control.Lens (Fold)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty, groupBy, head)
import Data.Text (Text, unwords, words)
import Prelude hiding (head, unwords, words)

groupByPairs :: (Eq b, Foldable f) => (a -> b) -> f a -> [(b, NonEmpty a)]
groupByPairs f xs = (\x -> (f (head x), x)) <$> groupBy ((==) `on` f) xs

between :: Ord a => a -> a -> a -> Bool
between a b x = x >= a && x <= b

worded :: Fold Text Text
worded f s = unwords <$> traverse f (words s)
