module Util(cleanString) where

import           Data.Char

cleanString :: String -> String
cleanString = reverse . cleanSimple . reverse . cleanSimple
    where
        shouldDrop x = (not . isPrint $ x)
                    || (isSpace x)
                    || (x == '\194')
        cleanSimple  = dropWhile shouldDrop
