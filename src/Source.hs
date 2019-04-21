{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Source ( FictionSource(..)
              , ChapterContent(..)
              , FictionInfo(..)
              , Fiction
              , chapter
              , chapterPG
              , fiction
              , dummyChapter
              , dummyFiction
              ) where

import           Control.Monad.Fraxl
import           Control.Monad.IO.Class
import           Data.Maybe
import           System.Console.AsciiProgress (ProgressBar)

type Fiction = (FictionInfo, [ChapterContent])

data ChapterContent = ChapterContent { chapterName    :: String
                                     , chapterContent :: [String]
                                     }
                    deriving (Show)

data FictionInfo = FictionInfo { fictionTitle      :: String
                               , fictionAuthor     :: String
                               , fictionChapterIDs :: [String]
                               }
                    deriving (Show)

data FictionSource a where
    Chapter :: String -> Maybe ProgressBar -> FictionSource ChapterContent
    Fiction :: String -> FictionSource FictionInfo

dummyFiction = FictionInfo    "Dummy Title" "Dummy Author" ["1"]
dummyChapter = ChapterContent "Dummy Title" ["Dummy Content"]

chapter cid = dataFetch $ Chapter cid Nothing
chapterPG pg cid = dataFetch $ Chapter cid (Just pg)
fiction fid = dataFetch $ Fiction fid
