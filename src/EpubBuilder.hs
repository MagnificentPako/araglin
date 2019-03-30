module EpubBuilder where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map                   as M
import           Source
import           Text.Pandoc

buildEpub :: (FictionInfo, [ChapterContent]) -> IO B.ByteString
buildEpub info = do
    res <- runIO $ writeEPUB2 options epubData
    handleError $ res
    where
        options  = def WriterOptions
        meta' = M.fromList [ ("title",  MetaString . fictionTitle  . fst $ info)
                           , ("author", MetaString . fictionAuthor . fst $ info)
                           ]
        meta = Meta {unMeta = meta'}
        epubData = Pandoc meta $ concat .
            map (\x ->
                [Header 1 nullAttr [Str $ chapterName x]]
                ++ concat (map (\y -> [Para [Str y]]) $ chapterContent x)
            ) $ snd info
