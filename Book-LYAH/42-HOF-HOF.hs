-- http://learnyouahaskell.com/higher-order-functions#higher-orderism
--

-- Now we're going to use higher order programming to implement a really useful function
-- that's in the standard library.
-- It's called zipWith.
-- It takes a function and two lists as parameters and then joins the two lists by applying
-- the function between corresponding elements.
--
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = (f a b) : (zipWith' f as bs)


-- All the things our zipWith can do:
ex_zipWith_1 = zipWith' max [6,3,2,1] [7,3,1,5]
ex_zipWith_2 = zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
ex_zipWith_3 = zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]