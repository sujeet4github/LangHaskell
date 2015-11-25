-- http://learnyouahaskell.com/for-a-few-monads-more#useful-monadic-functions
--

-- liftM and friends
{-
every monad is an applicative functor and every applicative functor is a functor.
But even though Monad should have the same constraint for Applicative, as every
monad is an applicative functor, it doesn't, because the Monad type class was
introduced to Haskell way before Applicative.


liftM :: (Monad m) => (a -> b) -> m a -> m b
vs
fmap :: (Functor f) => (a -> b) -> f a -> f b

ap :: (Monad m) => m (a -> b) -> m a -> m b
vs
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

many times when a type is found to be a monad, people first write up a Monad
instance and then make an Applicative instance by just saying that
    pure is return and <*> is ap.
Similarly, if you already have a Monad instance for something, you can give it
a Functor instance just saying that
    fmap is liftM.

-}

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

type Stack = [Int]

pop :: Stack -> (Int,Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((),a:xs)

-- reference implementation
stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((),newStack1) = push 3 stack
    (a ,newStack2) = pop newStack1
    in pop newStack2

{-
Notice how stackManip is itself a stateful computation.
We've taken a bunch of stateful computations and we've sort of glued them
together.

The above code for stackManip is kind of tedious since we're manually giving
the state to every stateful computation and storing it and then giving it
to the next one.

-}

-- TEXTBOOK EXAMPLE IS NOT WORKING


{-

Join Function:
join :: (Monad m) => m (m a) -> m a

So it takes a monadic value within a monadic value and gives us just a monadic
value, so it sort of flattens it...
for lists, join is just concat
-- For monads
m >>= f == join (fmap f m)
-}
ex_join= join (Just (Just 9))


{-
filterM:

filter :: (a -> Bool) -> [a] -> [a]
The predicate takes an element of the list and returns a Bool value.
Now, what if the Bool value that it returned was actually a monadic value?
Whoa! That is, what if it came with a context? Could that work?
For instance, what if every True or a False value that the predicate produced
also had an accompanying monoid value, like ["Accepted the number 5"] or
["3 is too small"]? That sounds like it could work. If that were the case,
we'd expect the resulting list to also come with a log of all the log values
that were produced along the way. So if the Bool that the predicate returned
came with a context, we'd expect the final resulting list to have some
context attached as well, otherwise the context that each Bool came with would
be lost.
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
-}
keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

test_filter_filterM = lhsSimple == rhsMonadicWithContextOfLog
    where
        lhsSimple = filter (\x -> x < 4) [9,1,5,2,10,3]
        rhsMonadicWithContextOfLog = fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]

test_filterM = mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
