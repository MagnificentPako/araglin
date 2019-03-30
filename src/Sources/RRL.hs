{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}


module Sources.RRL (fetchRRL) where

import           Control.Lens               ((^.))
import           Control.Monad.Fraxl
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as B (unpack)
import           Data.Char                  (isPrint)
import           Data.List
import           Data.List.Split            (splitOn)
import           Network.Wreq
import           Source
import           Text.HTML.TagSoup
import           Util

fetchRRL :: MonadIO m => Fetch FictionSource m a
fetchRRL = simpleAsyncFetch simpleFetch
    where
        simpleFetch :: FictionSource a -> IO a
        simpleFetch (Chapter cid) = fetchChapter cid
        simpleFetch (Fiction fid) = fetchFiction fid

fetchFiction fid = do
    rsp <- get $ "https://www.royalroad.com/fiction/" ++ fid
    let body     = B.unpack $ rsp ^. responseBody
        tags     = parseTags body
        author   = extractAuthor tags
        title    = extractTitle tags
        chapters = extractChapterIDs tags
    return $ FictionInfo { fictionTitle      = title
                         , fictionAuthor     = author
                         , fictionChapterIDs = chapters
                         }

fetchChapter cid = do
    rsp <- get $ "https://www.royalroad.com/fiction/chapter/" ++ cid
    let body    =  B.unpack $ rsp ^. responseBody
        tags    = parseTags body
        content = extractChapterContent tags
        title   = extractChapterTitle tags
    return $ ChapterContent { chapterName = title
                            , chapterContent = content
                            }

extractChapterIDs = map ((!! 5) . splitOn "/")
                  . filter ((1 <=) . length)
                  . map (fromAttrib "href" . (!! 0) . take 1)
                  . sections (isTagOpenName "a")
                  . takeWhile (~/= TagOpen "div" [("class", "portlet light reviews")])
                  . dropWhile (~/= TagOpen "table" [("id", "chapters")])

extractAuthor = innerText
              . take 3
              . dropWhile (~/= TagOpen "span" [("property", "name")])

extractTitle = innerText
             . take 2
             . dropWhile (~/= TagOpen "h1" [("property", "name")])

extractChapterTitle = cleanString
                    . innerText
                    . take 2
                    . dropWhile (~/= TagOpen "h1" [("class", "font-white"), ("style", "margin-top: 10px")])

extractChapterContent = filter ((> 1) . length)
                      . map (cleanString . innerText . return)
                      . takeWhile (~/= TagOpen "" [("class", "bold uppercase text-center")])
                      . dropWhile (~/= TagOpen "div" [("class", "chapter-inner chapter-content")])
