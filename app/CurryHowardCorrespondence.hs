{-# LANGUAGE OverloadedStrings #-}

module CurryHowardCorrespondence
    ( getCorrespondence
    , curryHowardCorrespondence
    ) where

import Prelude hiding (unwords)

import Control.Monad (msum)
import Data.Foldable (any, find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unwords, toLower)

type URL = Text

getCorrespondence :: Text -> Text
getCorrespondence = fromMaybe "unknown..." . lookupCorrespondence

lookupCorrespondence :: Text -> Maybe Text
lookupCorrespondence txt = msum $ fmap match curryHowardCorrespondence
  where
    txt' = toLower txt
    match (as, bs, url)
      | any ((==) txt' . toLower) as = appendUrl url <$> safeHead bs
      | any ((==) txt' . toLower) bs = appendUrl url <$> safeHead as
      | otherwise = Nothing

appendUrl :: URL -> Text -> Text
appendUrl url = unwords . (: [url])

safeHead :: [a] -> Maybe a
safeHead = find (const True)

curryHowardCorrespondence :: [([Text], [Text], URL)]
curryHowardCorrespondence =
  [ ( ["Natural Deducation", "自然演繹"]
    , ["Typed Lambda Calclus", "型付きラムダ計算", "単純型付きラムダ計算", "型付きλ計算", "単純型付きλ計算"]
    , "http://disi.unitn.it/~bernardi/RSISE11/Papers/curry-howard.pdf")
  , ( ["Sequent Calculus", "シーケント計算"]
    , ["Typed Lambda Calclus", "型付きラムダ計算", "単純型付きラムダ計算", "型付きλ計算", "単純型付きλ計算"]
    , "http://disi.unitn.it/~bernardi/RSISE11/Papers/curry-howard.pdf")
  , ( ["System F"]
    , ["Polymorphic Lambda Calculus", "2階ラムダ計算", "多相ラムダ計算", "2階λ計算", "多相λ計算"]
    , "http://disi.unitn.it/~bernardi/RSISE11/Papers/curry-howard.pdf")
  , ( ["Modal Logic", "様相論理"]
    , ["Mondad", "モナド"]
    , "http://www.sciencedirect.com/science/article/pii/S0304397596001697")
  , ( ["Linear Logic", "線形論理"]
    , ["Session Type", "セッション型"]
    , "http://ctp.di.fct.unl.pt/~btoninho/mscs12.pdf")
  ]
