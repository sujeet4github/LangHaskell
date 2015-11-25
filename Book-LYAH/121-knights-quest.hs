-- http://learnyouahaskell.com/a-fistful-of-monads#the-list-monad
--

{-
Say you have a chess board and only one knight piece on it. We want to find out if the knight
can reach a certain position in three moves. We'll just use a pair of numbers to represent the
knight's position on the chess board. The first number will determine the column he's in and
the second number will determine the row.


-}

import Control.Monad ((<=<))

type KnightPos = (Int,Int)

----------------------------------------------------------------------------------------------
-- The knight can always take
-- one step horizontally or vertically
-- and two steps horizontally or vertically
-- but its movement has to be both horizontal and vertical.

-- the result is non-deterministic :-) []
moveKnight_v1 (c,r) = [(c+hm, r+vm)
                        | hm <- [-2..2],
                          vm <- [-2..2],
                          hm /= 0,
                          vm /= 0,
                          abs(hm) /= abs(vm),
                          r+vm > 0,
                          r+vm <= 8,
                          c+hm > 0,
                          c+hm <= 8]

moveKnight_v2 (c,r) = filter onBoard allMoves
    where
        allMoves = [
            -- 2 horizontal moves, 1 vertical move
            (c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1),
            -- 1 horizontal move, 2 vertical moves
            (c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)
            ]
        onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- for lists
instance MonadPlus [] where
    mzero = []
    mplus = (++)

moveKnight_withMonadPlusList (c,r) =
    do
        (c',r') <- [
            -- 2 horizontal moves, 1 vertical move
            (c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1),
            -- 1 horizontal move, 2 vertical moves
            (c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)
            ]
        guard (c' `elem` [1..8] && r' `elem` [1..8])
        return (c', r')

moveKnight :: KnightPos -> [KnightPos]
moveKnight = moveKnight_withMonadPlusList
----------------------------------------------------------------------------------------------

-- takes a position and returns all the positions that you can reach from it in three moves:
in3 :: KnightPos -> [KnightPos]
in3 = in3_bind

in3_doNotation start = do
                        first <- moveKnight start
                        second <- moveKnight first
                        third <- moveKnight second
                        return third

in3_bind start = return start >>= moveKnight >>= moveKnight >>= moveKnight

----------------------------------------------------------------------------------------------

-- takes two positions and tells us if you can get from one to the other in exactly three steps
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start stop = stop `elem` in3 start


-- http://learnyouahaskell.com/for-a-few-monads-more#useful-monadic-functions
-- Composing monadic functions
-- :t (<=<)
-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachInN n start stop = stop `elem` inMany n start

----------------------------------------------------------------------------------------------

-- As an exercise, you can change this function so that when you can reach one position from the
-- other, it tells you which moves to take.

-- takes a position and returns all the positions that you can reach from it in three moves:
reachDestIn3 :: KnightPos -> KnightPos -> [[KnightPos]]
reachDestIn3 start stop = removeDuplicates (in3_checkDest_do start stop)

in3_checkDest_do start dest = do
                        first <- moveKnight start
                        second <- moveKnight first
                        guard (second /= start)
                        third <- moveKnight second
                        guard (third /= start)
                        guard (third /= first)
                        guard (first == dest || second == dest || third == dest)
                        if (first == dest)
                            then return [start, first]
                        else if (second == dest)
                            then return [start, first, second]
                        else
                            return [start, first, second, third]

remove :: Eq a => a -> [a] -> [a]
remove _ []     = []
remove x (y:xs)
    | x == y    = remove x xs
    | otherwise = y : (remove x xs)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where
        rdHelper seenElements [] = seenElements
        rdHelper seenElements (x:xs)
            | x `elem` seenElements = rdHelper seenElements xs
            | otherwise             = rdHelper (seenElements++[x]) xs

-- Without monads, list comprehensions - pretty hard to do....
in3_checkDest_simple start dest =
    let
        firstMoves =  moveKnight start
        secondMoves = map (remove (6,2) . moveKnight) firstMoves
    in undefined