module Epub.Elements where

import           Data.Either
import           Text.XML.HaXml
import           Text.XML.HaXml.Posn
import           Text.XML.HaXml.Pretty

strAtt = AttValue . return . Left
att = (. strAtt) . (,) . N

simpleElement name = Elem (N name)

celem x = CElem x noPos
cstr x  = CString False x noPos

container = simpleElement "container" [ att "version" "1.0"
                                       , att "xmlns" "urn:oasis:names:tc:opendocument:xmlns:container" ]

rootfiles  = celem . rootfiles'
rootfiles' = simpleElement "rootfiles" []

rootfile = (celem .) . rootfile'
rootfile' p t = simpleElement "rootfile" [ att "full-path" p, att "media-type" t ] []

package uid = simpleElement "package" [ att "version" "2.0"
                                      , att "xmlns" "http://www.idpf.org/2007/opf"
                                      , att "unique-identifier" uid]

metadata  = celem . metadata'
metadata' = simpleElement "metadata" [ att "xmlns:dc" "http://purl.org/dc/elements/1.1/"
                                     , att "xmlns:opf" "http://www.idpf.org/2007/opf"]

metaTitle x = celem $ simpleElement "dc:title" [] [ cstr x ]
metaLang  x = celem $ simpleElement "dc:language" [] [ cstr x ]
metaIdentifier bid scheme x = celem $ simpleElement "dc:identifier" [ att "id" bid
                                                                    , att "opf:scheme" scheme]
                                                                    [ cstr x ]
metaCreator x = celem $ simpleElement "dc:creator" [ att "opf:file-as" x
                                                   , att "opf:role" "aut"]
                                                   [ cstr x ]

manifest = celem . simpleElement "manifest" []
manifestItem iid href mtype = celem $ simpleElement "item"
    [ att "id" iid
    , att "href" href
    , att "media-type" mtype] []

spine = celem . simpleElement "spine" [ att "toc" "ncx" ]
itemRef x = celem $ simpleElement "itemref" [ att "idref" x] []

ncx = simpleElement "ncx" [ att "version" "2005-1"
                          , att "xml:lang" "en"
                          , att "xmlns" "http://www.daisy.org/z3986/2005/ncx/"]

ncxHead = celem . simpleElement "head" []
ncxMeta n c = celem $ simpleElement "meta" [ att "name" ("dtb:" ++ n)
                                           , att "content" c]
                                           []

text x = celem $ simpleElement "text" [] [ cstr x ]

docTitle x = celem $ simpleElement "docTitle" [] [ text x ]
docAuthor x = celem $ simpleElement "docAuthor" [] [ text x ]

navMap = celem . simpleElement "navMap" []
navContent x = celem $ simpleElement "content" [ att "src" x ] []
navLabel x = celem $ simpleElement "navLabel" [] [ text x ]
navPoint cls nid po e = celem $ simpleElement "navPoint" [ att "class" cls
                                                     , att "id" nid
                                                     , att "playOrder" po] e

html_ e = simpleElement "html" [ att "xmlns" "http://www.w3.org/1999/xhtml"
                                    , att "xml:lang" "en" ]
    [ ncxHead
        [ celem $ simpleElement "meta" [ att "http-equiv" "Content-Type"
                               , att "content" "application/xhtml+xml; charset=utf-8"] []
        , celem $ simpleElement "title" [] [ cstr "Title" ]
        , celem $ simpleElement "link" [ att "rel" "stylesheet"
                               , att "href" "main.css"
                               , att "type" "text/css" ] []]
    , celem $ simpleElement "body" [] e
    ]

h1_ x = celem $ simpleElement "h1" [] [ cstr x ]
p_ x = celem $ simpleElement "p" [] [ cstr x ]
