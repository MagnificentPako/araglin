{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Source ( FictionSource(..)
              , ChapterContent(..)
              , FictionInfo(..)
              , chapter
              , fiction
              ) where

import           Control.Monad.Fraxl
import           Control.Monad.IO.Class

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
    Chapter :: String -> FictionSource ChapterContent
    Fiction :: String -> FictionSource FictionInfo

chapter cid = dataFetch $ Chapter cid
fiction fid = dataFetch $ Fiction fid
