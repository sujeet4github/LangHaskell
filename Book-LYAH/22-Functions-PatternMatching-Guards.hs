-- http://learnyouahaskell.com/syntax-in-functions#guards-guards
--

factorial :: (Integral a) => a -> a
factorial 1 = 1
factorial n
        | n > 0 = n * factorial (n - 1)
        | otherwise = error "Factorial is not defined for this number"