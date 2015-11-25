-- http://learnyouahaskell.com/starting-out#im-a-list-comprehension
--

doubleFirst10Numbers = [x*2 | x <- [1..10]]

doubleFirst10NumbersOver12 = [x*2 | x <- [1..10], x*2 >= 12]
-- weeding out lists by predicates is also called filtering.
allNumbersFrom50To100WhoseRemainderWhenDividedBy7Is3 = [x | x <- [50..100], x `mod` 7 == 3]


-- we want a comprehension that replaces each odd number greater than 10 with "BANG!"
-- and each odd number that's less than 10 with "BOOM!".
-- If a number isn't odd, we throw it out of our list
boomBangs xs = take 20 [ if x > 10 then "BANG!" else "BOOM!" | x <- xs, odd x ]


productsOfAllCombinations = let xs = [2,5,10]
                                ys = [8,10,11]
                            in [ x*y | x <- xs, y <- ys]
allCombinations =   let nouns = ["hobo","frog","pope"]
                        adjectives = ["lazy","grouchy","scheming"]
                    in [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]


removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z'] ]


-- Nested list comprehensions are also possible if you're operating on lists that contain lists
nestedListComprehensions = let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
                            in [ [ x | x <- xs , even x ] | xs <- xxs ]