-- https://github.com/noelmarkham/learn-you-a-haskell-exercises/blob/master/4-syntax-in-functions.hs

-- This function should print a single digit number as English text,
-- or "unknown" if it's out of the range 0-9
englishDigit :: Int -> String
englishDigit x
    | x >= 0 && x <= 9  = show x
    | otherwise         = "unknown"

-- given a tuple, divide fst by snd, using pattern matching.
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, y) = if (y /= 0) then (x/y) else undefined

-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList xs = result
                    where
                        l = length xs
                        first3 = if (l > 3) then (take 3 xs) else [1,1,1]
