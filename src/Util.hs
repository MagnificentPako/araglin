module Util(cleanString) where

import           Data.Char

cleanString :: String -> String
cleanString = removeShit . reverse . cleanSimple . reverse . cleanSimple
    where
        shouldDrop x = (not . isPrint $ x)
                    || (isSpace x)
                    || (x == '\194')
        filters = ['\194', '\160']
        removeShit = filter (not . (`elem` filters))
        cleanSimple  = dropWhile shouldDrop
