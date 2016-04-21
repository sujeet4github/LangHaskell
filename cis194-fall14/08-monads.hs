{-
http://www.seas.upenn.edu/~cis194/fall14/lectures/08-monads.html

A monad is handy whenever a programmer wants to sequence actions, and the
details of the monad says exactly how the actions should be sequenced.
A monad may also store some information that can be read from and written
to while performing actions.

e.g.
IO,
[] - list,
Maybe,
Rand
-}

import Control.Monad

check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

ex01 = return 7 >>= check >>= halve
ex02 = return 12 >>= check >>= halve
ex03 = return 12 >>= halve >>= check
-- The do notation we’ve learned for working with IO can work with any monad:
ex04 = do
  checked <- check 7
  halve checked
ex05 = do
  checked <- check 12
  halve checked
ex06 = do
  halved <- halve 12
  check halved


addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

ex07 = [10,20,30] >>= addOneOrTwo
ex08 = do
  num <- [10, 20, 30]
  addOneOrTwo num

ex09 = do
  num <- [1..20]
  guard (even num)
  guard (num `mod` 3 == 0)
  return num

{-
Monad Combinators:

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma:mas) =
  ma >>= \a ->
  sequence mas >>= \as ->
  return (a:as)

Using sequence we can also write other combinators, such as

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = do
  x'  <- f x
  xs' <- mapM f xs
  return (x' : xs')

void :: Monad m => m a -> m ()
void ma = do
  _ <- ma
  return ()

join :: Monad m => m (m a) -> m a
join mma = do
  ma <- mma
  ma

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
  a <- ma
  return (f a)

when :: Monad m => Bool -> m () -> m ()
when b action =
  if b
  then action
  else return ()


NOTE: List Comprehensions
The monad for lists gives us a new notation for list building that turns out
to be quite convenient. Building lists using monad-like operations is so useful
that Haskell has a special syntax for it, called list comprehensions.

In turns out that there is a straightforward translation from list comprehensions to do notation:

[ a | b <- c, d, e, f <- g, h ]
is exactly equivalent to

do b <- c
   guard d
   guard e
   f <- g
   guard h
   return a

Note that, in the translation, lists aren’t mentioned anywhere!
With the GHC language extension MonadComprehensions, you can use list comprehension notation for
any monad. But, I’ve never used one for anything other than lists.

-}