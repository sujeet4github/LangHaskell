-- http://learnyouahaskell.com/syntax-in-functions#pattern-matching
--

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

-- You can use pattern match in list comprehensions. Check this out:
listComprehension_patternMatch =
    let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
        in [a+b | (a,b) <- xs]

-- get a first and a last name and give someone back their initials.
initials :: [Char] -> [Char] -> [Char]
initials (x:xs) (y:ys) = [x,'.',' ',y,'.']