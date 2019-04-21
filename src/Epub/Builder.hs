module Epub.Builder  where

import           Codec.Archive.Zip
import qualified Codec.Binary.UTF8.String   as UTF8
import qualified Data.ByteString.Lazy.Char8 as B
import           Epub.XML
import           Source

mkEntry p c = toEntry p 0 (B.pack c)


archiveFromFiction :: Fiction -> String -> B.ByteString
archiveFromFiction f uuid = fromArchive $ emptyArchive { zEntries =
    [ mkEntry "mimetype" "application/epub+zip"
    , mkEntry "META-INF/container.xml" $ mkContainer
    , mkEntry "OEBPS/content.opf" $ mkOpf f uuid
    , mkEntry "OEBPS/toc.ncx" $ mkNcx f uuid
    , mkEntry "OEBPS/main.css" $ ""
    ] ++ map (mkChapterEntry) (zip (snd f) [1..length (snd f)]) }
    where
        mkChapterEntry (c,n) = mkEntry ("OEBPS/chapter" ++ show n ++ ".xhtml") $ (UTF8.utf8Encode $ mkChapter c)
