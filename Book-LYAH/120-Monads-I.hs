-- http://learnyouahaskell.com/a-fistful-of-monads
--

{-

When we first talked about functors, we saw that they were a useful concept
for values that can be mapped over.

Then, we took that concept one step further by introducing applicative functors,
which allow us to view values of certain data types as values with contexts and
use normal functions on those values while preserving the meaning of those contexts.

In this chapter, we'll learn about monads, which are just beefed up applicative
functors, much like applicative functors are only beefed up functors.

maps a function (a -> b) over some data type (f a), to end up with (f b)
fmap :: (Functor f) => (a -> b) -> f a -> f b

Then we saw a possible improvement of functors and said, hey, what if that
function a -> b is already wrapped inside a functor value? Like, what if we
have Just (*3), how do we apply that to Just 5?

(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

We also saw that we can take a normal value and wrap it inside a data type.
For instance, we can take a 1 and wrap it so that it becomes a Just 1. Or we
can make it into a [1]. Or an I/O action that does nothing and just yields 1.
The function that does this is called pure.

Monads are a natural extension of applicative functors and with them we're
concerned with this:
if you have a value with a context, m a, how do you apply to it a function
that takes a normal a and returns a value with a context?
That is, how do you apply a function of type a -> m b to a value of type m a?
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
We use m a instead of f a because m stands for monad.
>>= is called bind.

-- monads came before applicatives were added to haskell
class Monad m where
    -- not like return in other languages, it just takes a normal value and puts
    -- it in a context.
    -- same as Applicative - pure
    return :: a -> m a

    -- bind
    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    -- We never use it explicitly in our code. Instead, it's used by Haskell to
    -- enable failure in a special syntactic construct for monads that we'll meet
    -- later.
    fail :: String -> m a
    fail msg = error msg

-}


-- Walk the line example
-------------------------

type Birds = Int
type Pole = (Birds, Birds)

-- a function that takes a number of birds and lands them on one side of the pole.
landLeft :: Birds -> Pole -> Pole
landLeft b (l, r) = (l + b, r)
landRight :: Birds -> Pole -> Pole
landRight b (l, r) = (l, r + b)

-- function to enable....
x -: f = f x

outOfBalance :: Birds -> Birds -> Bool
outOfBalance l r = abs(l - r) < 4

landLeftAndCheckBalance :: Birds -> Pole -> Maybe Pole
landLeftAndCheckBalance b (l, r)
    | outOfBalance (l+b) r    = Just (l + b, r)
    | otherwise                 = Nothing
landRightAndCheckBalance :: Birds -> Pole -> Maybe Pole
landRightAndCheckBalance b (l, r)
    | outOfBalance l (r+b)    = Just (l, r + b)
    | otherwise                 = Nothing

{-
try
ghci> return (0,0)
        >>= landLeftAndCheckBalance 1
        >>= landRightAndCheckBalance 4
        >>= landLeftAndCheckBalance (-1)
        >>= landRightAndCheckBalance (-2)

We couldn't have achieved this by just using Maybe as an applicative. If you try it,
you'll get stuck, because applicative functors don't allow for the applicative values
to interact with each other very much. They can, at best, be used as parameters to a
function by using the applicative style. The applicative operators will fetch their
results and feed them to the function in a manner appropriate for each applicative and
then put the final applicative value together, but there isn't that much interaction
going on between them. Here, however, each step relies on the previous one's result.
On every landing, the possible result from the previous one is examined and the pole
is checked for balance. This determines whether the landing will succeed or fail.
-}

-- always fail
banana :: Pole -> Maybe Pole
banana _ = Nothing

{-
try
ghci> return (0,0)
        >>= landLeftAndCheckBalance 1
        >>= banana
        >>= landRightAndCheckBalance 1

Instead of making functions that ignore their input and just return a predetermined
monadic value, we can use the >> function, whose default implementation works fine
in most cases.

The following works like above
ghci> return (0,0) >>= landLeftAndCheckBalance 1
        >> Nothing >>= landRightAndCheckBalance 1

-}


-- The do notation
------------------

{-
Consider this familiar example of monadic application:
ghci> Just 3 >>= (\x -> Just (show x ++ "!"))
Now, what if we had another >>= inside that function?
ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

We can replace this by:
foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)
"syntactic sugar" for above series of >>=

In a do expression, every line is a monadic value.
    To inspect its result, we use <-.
The last monadic value in a do expression, like Just (show x ++ y) here, can't be
used with <- to bind its result, because that wouldn't make sense if we translated
the do expression back to a chain of >>= applications.
Rather, its result is the result of the whole glued up monadic value, taking into
account the possible failure of any of the previous ones.

-}
--tightrope walkers routine in do notation
routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeftAndCheckBalance 2 start
    second <- landRightAndCheckBalance 2 first
    landLeftAndCheckBalance 1 second

routineWithBananaPeel :: Maybe Pole
routineWithBananaPeel = do
    start <- return (0,0)
    first <- landLeftAndCheckBalance 2 start
    -- note below
    Nothing
    second <- landRightAndCheckBalance 2 first
    landLeftAndCheckBalance 1 second

-- When we write a line in do notation without binding the monadic value with <-,
-- it's just like putting >> after the monadic value whose result we want to ignore.
-- We sequence the monadic value but we ignore its result because we don't care what
-- it is and it's prettier than writing
--    _ <- Nothing, which is equivalent to the above.
--

-- Pattern matching in a do expression
justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

-- When pattern matching fails in a do expression, the fail function is called. It's
-- part of the Monad type class and it enables failed pattern matching to result in a
-- failure in the context of the current monad instead of making our program crash.
-- Its default implementation is this:
-- fail :: (Monad m) => String -> m a
-- fail msg = error msg
-- So by default it does make our program crash, but monads that incorporate a context
-- of possible failure (like Maybe) usually implement it on their own.
-- For Maybe, its implemented like so
-- fail _ = Nothing
-- It ignores the error message and makes a Nothing. So when pattern matching fails in
-- a Maybe value that's written in do notation, the whole value results in a Nothing.
wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x


{-
for lists
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []

Lists represent non-determinism ....
[]      - failure
[1,2]   - 1 or 2

The following three are equivalent:

[1]
*Main> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

[2]
*Main> do
*Main| x <- [1,2]
*Main| y <- ['a','b']
*Main| return (x,y)
*Main|
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

[3]
*Main>[ (n,ch) | n <- [1,2], ch <- ['a','b'] ]

List comprehensions are just syntactic sugar for using lists as monads.
In the end, list comprehensions and lists in do notation translate to using >>= to do
computations that feature non-determinism.

-}


-- Monads Plus (Monads that are also Monoids)
---------------------------------------------

{-

List comprehensions allow us to filter our output.

To see how filtering in list comprehensions translates to the list monad, we have to
check out the guard function and the MonadPlus type class


-}

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

-- If guard succeeds, the result contained within it is an empty tuple. So then, we use
--  >> to ignore that empty tuple and present something else as the result. However, if
-- guard fails, then so will the return later on, because feeding an empty list to a
-- function with >>= always results in an empty list.
-- A guard basically says:
--  if this boolean is False then produce a failure right here,
--  otherwise make a successful value that has a dummy result of () inside it.
-- All this does is to allow the computation to continue.
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- for lists
instance MonadPlus [] where
    mzero = []
    mplus = (++)

-- Using guard in the list monad to filter
sevensOnly_bind = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
-- same using the do notation
sevensOnly_do :: [Int]
sevensOnly_do = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x
-- list comprehension
sevensOnly_lc = [ x | x <- [1..50], '7' `elem` show x ]

-- Knights Quest
-- 121-knights-quest.hs

-- Monad Laws
--------------------------------------------
-- http://learnyouahaskell.com/a-fistful-of-monads#monad-laws

-- left identity
-- return x >>= f   ==  f x
-- right identity
-- m >>= return     ==  m
-- associativity
-- (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
