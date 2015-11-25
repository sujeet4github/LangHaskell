{-# OPTIONS_GHC -Wall #-}

-- Lazy Evaluation
-- http://www.seas.upenn.edu/~cis194/spring13/lectures/06-laziness.html

-- Suggested Reading
-- foldr foldl foldl’ (http://haskell.org/haskellwiki/Foldr_Foldl_Foldl%27) from the Haskell wiki
-- We are going to define our own folds so we hide the ones from the Prelude:
import Prelude hiding (foldr, foldl)
-- goes on to give with examples - rules of thumb regarding folds
-- 1st choice foldr
-- 2nd choice foldl' - this gives better space and time performance over foldr
-- foldl only in rare situations
--
--
import Data.Array

-- Under a strict evaluation strategy, function arguments are completely
-- evaluated before passing them to the function.
-- In Haskell:
--  The slogan to remember is “pattern matching drives evaluation”.
--    Expressions are only evaluated when pattern-matched
--    ...only as far as necessary for the match to proceed, and no farther!
--
-- e.g.
(&&) :: Bool -> Bool -> Bool
(&&) True x = x
(&&) False _ = False
-- Notice how this definition of (&&) does not pattern-match on its second argument. Moreover, if the first argument is False, the second argument is entirely ignored. Since (&&) does not pattern-match on its second argument at all, it is short-circuiting in exactly the same way as the && operator in Java or C++.
-- Notice that (&&) also could have been defined like this:
--
(&&!) :: Bool -> Bool -> Bool
(&&!) True True  = True
(&&!) True False = False
(&&!) False True  = False
(&&!) False False = False
fast_false :: Bool
fast_false = False Main.&&  (34^9784346 > 34987345)
slow_false :: Bool
slow_false = False Main.&&! (34^9784346 > 34987345)
-- These will both evaluate to False, but the second one will take a whole
-- lot longer! Or how about this:
will_not_crash = False Main.&&  (head [] == 'x')
will_crash = False &&! (head [] == 'x')

-- foldl performance Discussion
-- Standard library function foldl, provided for reference
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs
-- Let’s consider how evaluation proceeds when we evaluate foldl (+) 0 [1,2,3]
-- (which sums the numbers in a list):
--
--  foldl (+) 0 [1,2,3]
-- = foldl (+) (0+1) [2,3]
-- = foldl (+) ((0+1)+2) [3]
-- = foldl (+) (((0+1)+2)+3) []
-- = (((0+1)+2)+3)
-- = ((1+2)+3)
-- = (3+3)
-- = 6
--
-- Since the value of the accumulator is not demanded until recursing through
-- the entire list, the accumulator simply builds up a big unevaluated expression
-- (((0+1)+2)+3), which finally gets reduced to a value at the end.
-- 2 problems with this:
--  One is that it’s simply inefficient:
--     there’s no point in transferring all the numbers from the list into a
--     different list-like thing (the accumulator thunk) before actually adding
--     them up.
--  The second problem is more subtle, and more insidious:
--     evaluating the expression (((0+1)+2)+3) actually requires pushing the 3
--     and 2 onto a stack before being able to compute 0+1 and then unwinding
--     the stack, adding along the way. This is not a problem for this small
--     example, but for very long lists it’s a big problem: there is usually
--     not as much space available for the stack, so this can lead to a stack
--     overflow.
--
-- The solution in this case is to use the foldl' function instead of foldl,
-- which adds a bit of strictness: in particular, foldl' requires its second
-- argument (the accumulator) to be evaluated before it proceeds,
--  so a large thunk never builds up:
--
-- foldl' (+) 0 [1,2,3]
-- = foldl' (+) (0+1) [2,3]
-- = foldl' (+) 1 [2,3]
-- = foldl' (+) (1+2) [3]
-- = foldl' (+) 3 [3]
-- = foldl' (+) (3+3) []
-- = foldl' (+) 6 []
-- = 6
--
-- How does foldl' do this?
--  seq - http://www.haskell.org/haskellwiki/Seq
--

-- 0-1 knapsack problem - http://en.wikipedia.org/wiki/Knapsack_problem
--
-- Note how we simply define the array m in terms of itself,
-- using the standard recurrence, and let lazy evaluation work out the proper
-- order in which to compute its cells.
--
knapsack01 :: [Double]   -- values
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack01 vs ws maxW = m ! (numItems-1, maxW)
             where numItems = length vs
                   m = array ((-1,0), (numItems-1, maxW)) $
                         [((-1,w), 0) | w <- [0 .. maxW]] ++
                         [((i,0), 0) | i <- [0 .. numItems-1]] ++
                         [((i,w), best)
                             | i <- [0 .. numItems-1]
                             , w <- [1 .. maxW]
                             , let best
                                     | ws!!i > w  = m!(i-1, w)
                                     | otherwise = max (m!(i-1, w))
                                                       (m!(i-1, w - ws!!i) + vs!!i)
                         ]

example = knapsack01 [3,4,5,8,10] [2,3,4,5,9] 20
