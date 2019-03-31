{-# LANGUAGE GADTs #-}

module Sources.Dummy (fetchDummy) where

import           Control.Monad.Fraxl
import           Control.Monad.IO.Class
import           Source

fetchDummy :: MonadIO m => Fetch FictionSource m a
fetchDummy = simpleAsyncFetch simpleFetch
    where
        simpleFetch :: FictionSource a -> IO a
        simpleFetch (Chapter cid) = do
            return ChapterContent { chapterName    = "Dummy Chapter"
                                  , chapterContent = ["Dummy Content"]
                                  }
        simpleFetch (Fiction fid) = do
            return FictionInfo { fictionTitle      = "Dummy Fiction"
                               , fictionAuthor     = "Dummy Auhtor"
                               , fictionChapterIDs = ["Dummy"]
                               }
