{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad
import           Control.Monad.Fraxl
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8   as B
import           EpubBuilder
import qualified Network.HTTP.Client          as H
import qualified Network.HTTP.Client.TLS      as T
import qualified Network.Wreq.Session         as S
import           Source
import           Sources.Dummy
import           Sources.RRL
import           System.Console.AsciiProgress
import           System.Environment

main :: IO ()
main = displayConsoleRegions $ do
    args <- getArgs

    let mSettings = T.tlsManagerSettings
            { H.managerResponseTimeout    = H.responseTimeoutMicro 600000000
            , H.managerRetryableException = const True
            }
    session <- S.newSessionControl Nothing mSettings

    let fetcher :: MonadIO m => Fetch FictionSource m a
        fetcher = case (args !! 0) of
            "rrl"   -> fetchRRL session
            "dummy" -> fetchDummy
            _       -> fetchDummy
    fic <- runFraxl (fetcher) (fraxlFiction (args !! 1))
    epub <- buildEpub fic
    B.writeFile (args !! 2) epub

fraxlFiction fid = do
    f <- fiction $ fid
    pg <- liftIO $ newProgressBar def
        { pgTotal = toInteger . length . fictionChapterIDs $ f
        , pgFormat = "Downloading '" ++ fictionTitle f ++ "' :percent [:bar] (:current/:total)"
        , pgWidth = 100
        }
    c <- mapM (chapterPG pg) (fictionChapterIDs f)
    return (f,c)
