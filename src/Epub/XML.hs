module Epub.XML where

import qualified Data.ByteString.Lazy.UTF8   as B (toString)
import           Data.Maybe
import           Epub.Elements
import           Source
import           Text.HTML.TagSoup.Entity
import           Text.XML.HaXml
import           Text.XML.HaXml.ByteStringPP
import qualified Text.XML.HaXml.Pretty       as PP

xmlDecl = XMLDecl "1.0"
                  (Just $ EncodingDecl "UTF-8")
                  Nothing

ncxDoctype = DTD (N "ncx")
    (Just $ PUBLIC
        (PubidLiteral "-//NISO//DTD ncx 2005-1//EN")
        (SystemLiteral "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd")) []

htmlDoctype = DTD (N "html")
    (Just $ PUBLIC
        (PubidLiteral "-//W3C//DTD XHTML 1.1//EN")
        (SystemLiteral "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd")) []

prolog' dt = Prolog (Just xmlDecl)
                []
                dt
                []

doc e dt =
    Document
        (prolog' dt)
        emptyST
        e
        []

mkContainer :: String
mkContainer = render . PP.document $ doc cont Nothing
    where
        cont = container
            [ rootfiles [
                rootfile "OEBPS/content.opf" "application/oebps-package+xml"
            ]]


mkNcx :: Fiction -> String -> String
mkNcx (f,c) uuid = render . PP.document $ doc cont (Just ncxDoctype)
    where
        mkNavPoint (c,n) = navPoint "chapter" ("chapter" ++ show n) (show n) [ navLabel $ (chapterName c)
                                                                                     , navContent $ "chapter" ++ (show n) ++ ".xhtml" ]
        cont = ncx
            [ ncxHead [ ncxMeta "uid" "https://loli.church"
                    , ncxMeta "depth" "1"
                    , ncxMeta "totalPageCount" "0"
                    , ncxMeta "maxPageCount" "0" ]
            , docTitle (fictionTitle f)
            , docAuthor (fictionAuthor f)
            , navMap $ map (mkNavPoint) (zip c [1..length c])
            ]

mkOpf :: Fiction -> String-> String
mkOpf (f,c) uuid = render . PP.document $ doc cont Nothing
    where
        mkManifestItem x = manifestItem chn (chn ++ ".xhtml") "application/xhtml+xml"
            where
                chn = ("chapter" ++ (show $ snd x))
        cont = package uuid
            [ metadata [ metaTitle (fictionTitle f)
                        , metaLang "en"
                        , metaIdentifier uuid "URL" "https://loli.church"
                        , metaCreator (fictionAuthor f)]
            , manifest $ (map (mkManifestItem) $ zip c [1..length c]) ++
                [ manifestItem "ncx" "toc.ncx" "application/x-dtbncx+xml"
                , manifestItem "stylesheet" "main.css" "text/css"]
            , spine $ map (\x -> itemRef ("chapter" ++ show x)) [1..length c]
            ]

mkChapter :: ChapterContent -> String
mkChapter c = render . PP.document $ doc cont (Just htmlDoctype)
    where
        cont = html_ $ [h1_ $ chapterName c] ++ (map (p_ . escapeXML) $ chapterContent c)
