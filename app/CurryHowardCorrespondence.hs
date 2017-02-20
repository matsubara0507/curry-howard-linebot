{-# LANGUAGE OverloadedStrings #-}

module CurryHowardCorrespondence
    ( getCorrespondence
    , curryHowardCorrespondence
    ) where

import Prelude hiding (unwords)

import Control.Monad (msum)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unwords)

type URL = Text

getCorrespondence :: Text -> Text
getCorrespondence = fromMaybe "unknown..." . lookupCorrespondence

lookupCorrespondence :: Text -> Maybe Text
lookupCorrespondence txt = msum $ fmap match curryHowardCorrespondence
  where
    match (ax, bx, url)
      | txt `elem` ax = (unwords . (: [url])) <$> find (const True) bx
      | txt `elem` bx = (unwords . (: [url])) <$> find (const True) ax
      | otherwise = Nothing

curryHowardCorrespondence :: [([Text], [Text], URL)]
curryHowardCorrespondence =
  [ ( ["Natural Deducation", "自然演繹"]
    , ["Typed Lambda Calclus", "型付きラムダ計算"]
    , "http://disi.unitn.it/~bernardi/RSISE11/Papers/curry-howard.pdf")
  , ( ["Sequent Calculus", "シーケント計算"]
    , ["Typed Lambda Calclus", "型付きラムダ計算"]
    , "http://disi.unitn.it/~bernardi/RSISE11/Papers/curry-howard.pdf")
  ]
