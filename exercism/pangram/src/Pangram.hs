module Pangram (isPangram, isPangram2, isPangram3) where

import Data.Char (toLower, toUpper, isAlpha)

isPangram = isPangram3

isPangram1 :: String -> Bool
isPangram1 text = and $ map (\x -> elem x textInLowerCase) ['a' .. 'z']
                where
                    textInLowerCase = map toLower text

isPangram2 :: String -> Bool
isPangram2 text = and $ map (isElement textInLowerCase) ['a' .. 'z']
                where
                    textInLowerCase = map toLower text
isElement :: [Char] -> Char -> Bool
isElement xs x = elem x xs

isPangram3 :: String -> Bool
isPangram3 text = 26 == (length  $ nub' $ map toUpper $ filter isAlpha text)

nub' :: [Char] -> [Char]
nub' l = nubHelper l []
  where
    nubHelper [] _              = []
    nubHelper (x:xs) ls
      | x `elem` ls   = nubHelper xs ls
      | otherwise     = x : nubHelper xs (x:ls)
