module Bob (responseFor) where

import Data.Char (isLower, isSpace, isAlpha, isDigit)

data WhatBobHears = Question
                  | Yelling
                  | Silence
                  | Whatever
            deriving (Show)

responseFor :: String -> String
responseFor xs = case translateToWhatBobHears xs of
   Question -> "Sure."
   Yelling  -> "Whoa, chill out!"
   Silence  -> "Fine. Be that way!"
   Whatever -> "Whatever."

translateToWhatBobHears :: String -> WhatBobHears
translateToWhatBobHears xs
   | isYelling    xs = Yelling
   | isQuestion   xs = Question
   | isNothing    xs = Silence
   | otherwise       = Whatever

isYelling :: String -> Bool
isYelling xs = hasAlphaChars xs && allAlphaCharsAreUpperCase xs

isQuestion :: String -> Bool
isQuestion xs = case (dropWhile isSpace . reverse) xs of
  '?':xs  -> True
  otherwise -> False

hasAlphaChars :: String -> Bool
hasAlphaChars = not . null . (filter isAlpha)

allAlphaCharsAreUpperCase :: String -> Bool
allAlphaCharsAreUpperCase = not . or . map isLower . filter isAlpha

isNothing :: String -> Bool
isNothing = and . map isSpace
