{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( scrapeGameStatsFromUrl
  )
where

import           Control.Applicative
import           System.Environment
import           Text.HTML.Scalpel

type Team = String

data TeamScore = TeamScore Team Int
  deriving (Show, Eq)

-- type Date = String
-- type StartTime = String
-- this is just a list of tags for now, an extra function will be required
-- to parse certain things
data GameMeta = GameMeta [String]
    deriving (Show, Eq)

data GameStats = GameStats TeamScore TeamScore GameMeta
    deriving (Show, Eq)

gameScore :: Scraper String GameStats
gameScore = chroot ("div" @: ["id" @= "content"]) teamScores

teamScores :: Scraper String GameStats
teamScores = do
  scores    <- chroots ("div" @: [hasClass "scorebox"] // "div") teamScore
  game_meta <- chroot ("div" @: [hasClass "scorebox_meta"]) gameMeta
  return $ GameStats (head scores) (last scores) game_meta

teamScore :: Scraper String TeamScore
teamScore = do
  name  <- text $ "strong" // "a" @: ["itemprop" @= "name"]
  score <- text $ "div" @: [hasClass "scores"] // "div" @: [hasClass "score"]
  return $ TeamScore name (read score)

gameMeta :: Scraper String GameMeta
gameMeta = do
  meta <- chroots ("div" // "div") gameTags
  return $ GameMeta meta

gameTags :: Scraper String String
gameTags = do
  tag <- text $ "div"
  return $ tag

getStatsFromUrl :: URL -> IO ()
getStatsFromUrl url = do
  output <- scrapeURL url gameScore
  print output

scrapeGameStatsFromUrl :: IO ()
scrapeGameStatsFromUrl = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [url] = getStatsFromUrl url
handleArgs _     = putStrLn "usage: scout <URL>"
