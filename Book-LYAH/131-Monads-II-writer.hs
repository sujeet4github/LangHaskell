-- http://learnyouahaskell.com/for-a-few-monads-more#writer
--
{-
the Writer monad is for values that have another value attached that acts
as a sort of log value. Writer allows us to do computations while making
sure that all the log values are combined into one log value that then
gets attached to the result.

e.g.
-}

-- https://wiki.haskell.org/All_About_Monads
import Control.Monad.Writer

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

-- a function that takes a value with an attached log, that is, an (a,String)
-- value and a function of type a -> (b,String) and feeds that value into the
-- function.
-- But because an (a,String) value doesn't carry with it a context of possible
-- failure, but rather a context of an additional log value, applyLog is going
-- to make sure that the log of the original value isn't lost, but is joined
-- together with the log of the value that results from the function.
applyLog_v1 :: (a,String) -> (a -> (b,String)) -> (b,String)
applyLog_v1 (x,log) f =
    let
        (y, new_log) = f x
    in
        (y, log ++ new_log)
-- applyLog takes values of type (a,String), but is there a reason that the log
-- has to be a String? It uses ++ to append the logs, so wouldn't this work on
-- any kind of list, not just a list of characters? Sure it would. We can go ahead
-- and change its type to this:
applyLog_v2 :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])
applyLog_v2 (x,log) f =
    let
        (y, new_log) = f x
    in
        (y, log ++ new_log)

-- however ++ does not apply to bytestrings, however we can use a general Monoid...
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f =
    let
        (y, new_log) = f x
    in
        (y, log `mappend` new_log)
-- since the accompanying value can be any monoid value, we no longer have to think
-- of the tuple as a value and a log...

test_1 = (3, "Smallish gang.") `applyLog` isBigGang
test_2 = (30, "A freaking platoon.") `applyLog` isBigGang
test_3 = ("Bathcat","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length"))

{-
    Writer is defined as:

    newtype Writer w a = Writer { runWriter :: (a, w) } deriving (Show)
    -- defined as a new type rather than a tuple, so that it can be made an instance
    -- of Monad and that its type is separate from a normal tuple.
    -- its Monad instance is defined as:
    instance (Monoid w) => Monad (Writer w) where
        return x = Writer(x,mempty)
        (Writer(x,v)) >>= f =
            let
                Writer(y,v') = f x
            in
                Writer(y, v `mappend` v')

 - Note, There appear to have been some changes to ghc since Learn You a Haskell was written. There
 - is no data constructor exposed for Writer, like described in the chapter. You cannot do things
 - like: logNumber x = Writer (x, ["Got number: " ++ show x]). However there is a 'writer' function,
 - which works in the same way as that constructor would.
 - That is, you could do: logNumber x = writer (x, ["Got number: " ++ show x]).
 - For more information,
 -  see http://stackoverflow.com/questions/11684321/how-to-play-with-control-monad-writer-in-haskell
 -
 - That said, it is possible to create the Writers necessary for this exercise using only return and tell.

-}

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    -- Sometimes we just want some monoid value to be included at some particular point.
    -- For this, the tell function is useful. It's part of the MonadWriter type class and
    -- in the case of Writer it takes a monoid value, like ["This is going on"] and creates
    -- a Writer value that presents the dummy value () as its result but has our desired
    -- monoid value attached.
    tell ["Gonna multiply these two"]
    return (a*b)
multWithLogWithoutDo =
    logNumber 3 >>=
        (\x -> logNumber 5
            >>=
                (\y -> logNumber (x*y))
            >> tell ["Gonna multiply these two"]
        )

test_multWithLog = expected == actual
    where
        expected = (15,["Got number: 3","Got number: 5","Gonna multiply these two"])
        actual = runWriter multWithLog

--------------------------------------------------------------------------------------

--
-- Euclid's algorithm is an algorithm that takes two numbers and computes their
-- greatest common divisor. That is, the biggest number that still divides both of them.
-- Haskell already has a gcd function, lets write our own with logging capabilities
--
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0    = do
                    tell ["Finished with " ++ show a]
                    return a
    | otherwise = do
                    let r = (a `mod` b)
                    tell [show a ++ " mod " ++ show b ++ " = " ++ show r]
                    gcd' b r



--------------------------------------------------------------------------------------
-- Different Lists

{-

When using the Writer monad, you have to be careful which monoid to use, because using lists can sometimes
turn out to be very slow. That's because lists use ++ for mappend and using ++ to add something to the end
of a list is slow if that list is really long.

a ++ (b ++ (c ++ (d ++ (e ++ f))))
is much more efficient than:
((((a ++ b) ++ c) ++ d) ++ e) ++ f

In the first, we fully construct the left part of the list and only then add a longer list on the right.

In the second, every time it wants to add the right part to the left, it has to construct the left part
all the way from the beginning!

Because lists can sometimes be inefficient when repeatedly appended in this manner,
it's best to use a data structure that always supports efficient appending. One such data structure is
the difference list.

A difference list is similar to a list, only instead of being a normal list, it's a function that takes
a list and prepends another list to it.

-}

-- make a new type so we can make it into a monoid
-- note this is a function that makes an [a] with an [a]
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

-- conversion methods
-- not
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = (f [])

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

finalCountDown_faster :: Int -> Writer [String] ()
finalCountDown_faster 0 =
    do
        tell ["Done - 0"]
finalCountDown_faster x =
    do
        -- appends like this (a ++ (b ++ (c ++ (d ++ (e ++ f)))))
        tell [show x]
        finalCountDown_faster (x - 1)

finalCountDown_slower :: Int -> Writer [String] ()
finalCountDown_slower 0 =
    do
        tell ["Done - 0"]
finalCountDown_slower x =
    do
        -- do it this way to make it slower
        -- (((((a ++ b) ++ c) ++ d) ++ e) ++ f)
        --   rather than (a ++ (b ++ (c ++ (d ++ (e ++ f)))))
        finalCountDown_slower (x - 1)
        tell [show x]

finalCountDown_faster_2 :: Int -> Writer (DiffList String) ()
finalCountDown_faster_2 0 = do
                            tell (toDiffList ["Done - 0"])
finalCountDown_faster_2 x =
    do
        -- do it this way to make it slower
        -- (((((a ++ b) ++ c) ++ d) ++ e) ++ f)
        --   rather than (a ++ (b ++ (c ++ (d ++ (e ++ f)))))
        -- but now we are using DiffList rather than simple lists
        finalCountDown_faster_2 (x - 1)
        tell $ toDiffList [show x]

test_finalCountDown_faster = mapM_ putStrLn . snd . runWriter $ finalCountDown_faster 5000
test_finalCountDown_slower = mapM_ putStrLn . snd . runWriter $ finalCountDown_slower 5000
test_finalCountDown_faster_withDL = mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown_faster_2 5000
