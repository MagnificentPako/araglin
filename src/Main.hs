module Main where

import           Control.Monad
import           Control.Monad.Fraxl
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as B
import           EpubBuilder
import           Source
import           Sources.RRL
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    fic <- runFraxl (fetchRRL) $ do
        f <- fiction $ args !! 0
        c <- mapM (chapter) (fictionChapterIDs f)
        return (f,c)
    epub <- buildEpub fic
    B.writeFile (args !! 1) epub
