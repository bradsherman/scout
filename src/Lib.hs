{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import           Control.Applicative
import           Text.HTML.Scalpel

type Team = String

data TeamScore = TeamScore Team Int
    deriving (Show, Eq)

data GameScore = GameScore TeamScore TeamScore
    deriving (Show, Eq)

gameScore :: Scraper String [TeamScore]
gameScore = chroots ("div" @: [hasClass "scorebox"] // "div") teamScore

teamScore :: Scraper String TeamScore
teamScore = do
  name <- text $ "strong" // "a" @: ["itemprop" @= "name"]
  score <- text $ "div" @: [hasClass "scores"] // "div" @: [hasClass "score"]
  return $ TeamScore name (read score)

type Author = String
data Comment = TextComment Author String
    | ImageComment Author URL
    deriving (Show, Eq)

someFunc :: IO ()
someFunc = do
  output <- scrapeURL "https://www.baseball-reference.com/boxes/MIA/MIA201903280.shtml" gameScore
  print output
