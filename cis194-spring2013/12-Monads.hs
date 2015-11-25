-- Monads
-- http://www.seas.upenn.edu/~cis194/spring13/lectures/12-monads.html
--

-- Suggested Reading
-- http://www.haskell.org/haskellwiki/Typeclassopedia
-- LYAH - http://learnyouahaskell.com/a-fistful-of-monads
-- LYAH - http://learnyouahaskell.com/input-and-output
-- RWH - http://book.realworldhaskell.org/read/monads.html
-- RWH - http://book.realworldhaskell.org/read/programming-with-monads.html

{-
Applicative: idiomatically handle computations taking place in some sort of
"special context", for example:
    1. possible failure - Maybe, Either
    2. possible multiple outputs - []
    3. consulting some sort of environment - ((->) e)
    4. parser construction using "combinator" approach

So far, we have only seen computations with a fixed structure, applying a data
constructor to a fixed set of arguments.
What if we don’t know the structure of the computation in advance – that is, we
want to be able to decide what to do based on some intermediate results?

e.g.
suppose we are trying to parse a file containing a sequence of numbers, like this:

4 78 19 3 44 3 1 7 5 2 3 2

The catch is that the first number in the file tells us the length of a following
“group” of numbers; the next number after the group is the length of the next group,
and so on. So the example above could be broken up into groups like this:

78 19 3 44   -- first group
1 7 5        -- second group
3 2          -- third group
This is a somewhat contrived example, but in fact there are many “real-world” file
formats that follow a similar principle—you read some sort of header which then tells
you the lengths of some following blocks, or where to find things in the file, and so on.

We would like to write a parser for this file format of type

parseFile :: Parser [[Int]]
Unfortunately, this is not possible using only the Applicative interface.
The problem is that Applicative gives us no way to decide what to do next based on
previous results: we must decide in advance what parsing operations we are going to
run, before we see the results.

It turns out, however, that the Parser type can support this sort of pattern, which
is abstracted into the Monad type class.

The Monad type class is defined as follows:

class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>)  :: m a -> m b -> m b
  m1 >> m2 = m1 >>= \_ -> m2

return also looks familiar because it has the same type as pure. In fact, every Monad
should also be an Applicative, with pure = return.
The reason we have both is that Applicative was invented after Monad had already been
around for a while.

(>>) is just a specialized version of (>>=) (it is included in the Monad class in case
some instance wants to provide a more efficient implementation, but usually the default
implementation is just fine). So to understand it we first need to understand (>>=).

There is actually a fourth method called fail, but putting it in the Monad class was a
mistake, and you should never use it, so I won’t tell you about it (you can read about
it in the Typeclassopedia if you are interested).

(>>=) (pronounced “bind”) is where all the action is! Let’s think carefully about its
type:
(>>=) :: m a -> (a -> m b) -> m b

(>>=) takes two arguments.
The first one is a value of type m a.
(Incidentally, such values are sometimes called monadic values, or computations.
It has also been proposed to call them mobits. The one thing you must not call them
is “monads”, since that is a kind error: the type constructor m is a monad.)

-- instance for Maybe already defined in Prelude
instance Monad Maybe where
    return  = Just
    Nothing >>= _ = Nothing
    Just x  >>= k = k x
-- so is instance for List
instance Monad [] where
    return = [x]
    xs >>= k = concat (map k xs)

-}


check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

maybe_ex01 = return 7 >>= check >>= halve
maybe_ex02 = return 12 >>= check >>= halve
maybe_ex03 = return 12 >>= halve >>= check

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

list_ex01 = [10,20,30] >>= addOneOrTwo


{-
Monad Combinators
=================
One nice thing about the Monad class is that using only return and (>>=) we can build up a lot of nice
general combinators for programming with monads.

Example:
sequence takes a list of monadic values and produces a single monadic value which collects the results.
What this means depends on the particular monad.
For example,
    in the case of Maybe it means that the entire computation succeeds only if all the individual ones
    do;
    in the case of IO it means to run all the computations in sequence;
    in the case of Parser it means to run all the parsers on sequential parts of the input (and succeed
    only if they all do).

-- Already defined in Prelude
sequence :: Monad m => [m a] -> m [a]
sequence [] = []
sequence (ma:mas) =
    ma >>= \a ->
                sequence mas >>= \as ->
                                    return (a:as)

Using sequence, we can write other combinators like
-}
replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)


{-
now we are finally in a position to write the parser we wanted to write: it is simply

parseFile :: Parser [[Int]]
parseFile = many parseLine

parseLine :: Parser [Int]
parseLine = parseInt >>= \i -> replicateM i parseInt
(many was also known as zeroOrMore on the homework).

 -}
