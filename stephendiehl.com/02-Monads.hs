{-
See README.txt

Eightfold Path to Monad Satori
------------------------------
Much ink has been spilled waxing lyrical about the supposed mystique of
monads. Instead I suggest a path to enlightenment:
1. Don't read the monad tutorials.
2. No really, don't read the monad tutorials.
3. Learn about Haskell types.
4. Learn what a typeclass is.
5. Read the Typeclassopedia.
6. Read the monad definitions.
7. Use monads in real code.
8. Don't write monad-analogy tutorials.
In other words, the only path to understanding monads is to read the fine
source, fire up GHC and write some code. Analogies and metaphors will not
lead to understanding.

Monadic Myths
-------------
The following are all false:
1. Monads are impure.
2. Monads are about effects.
3. Monads are about state.
4. Monads are about imperative sequencing.
5. Monads are about IO.
6. Monads are dependent on laziness.
7. Monads are a "back-door" in the language to perform side-effects.
8. Monads are an embedded imperative language inside Haskell.
9. Monads require knowing abstract mathematics.
See: https://wiki.haskell.org/What_a_Monad_is_not

Monads are not complicated, the implementation is a typeclass with two
functions, (>>=) pronounced "bind" and return. Any preconceptions one
might have for the word "return" should be discarded, it has an
entirely ***different*** meaning.

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a

Do Notation
-----------
syntactic sugar.
The de-sugaring is defined recursively by following rules:
# using the >>= operator that throws away argument, do f and bind
# result with m
do { a <- f ; m }
== f >>= \a -> do { m }

# using the >> operator that throws away argument, do f andThen m
do { f ; m }
== f >> do { m }

do { m }
== m

Monad Laws:
#1:
    return a >>= f
    == f a

    OR (using do notation)

    do  y <- return x
        f y
    == do f x

#2:
    m >>= return
    == m

    OR (using do notation)

    do x <- m
         return x
    == do m

#3:
    (m >>= f) >>= g
    == m >>= (\x -> f x >>= g)

    OR (using do notation)

    do
        b <- do
                a <- m
                f a
        g b
    ==  do  a <- m
            b <- f a
            g b
    ==  do  a <- m
            do
                b <- f a
                g b

What Makes Monads in Haskell Hard?
----------------------------------
1. There are several levels on indirection with desugaring the do
    main = do
        x <- getLine
        putStrLn x
        return ()
is desugared to:
main =
    getLine >>= \x ->
                    putStrLn x >>= \_ -> return()

2. Asymmetric binary infix operators for higher order functions are not found
in other languages
    LHS of (>>=) is m a
    RHS is a -> m b
    The >>= is building up a much larger function by composing functions together.

main =
    (>>=)
        getLine
        (\x ->
            (>>=)
                putStrLn x
                (\_ -> return ())
        )
main =
    bind
        getLine
        (\x ->
            bind
                putStrLn x
                (\_ -> return ())
        )
    where
        bind x y = x >>= y

3. Ad-hoc polymorphism is not commonplace in other languages
Haskell's implementation of overloading can be unintuitive if one is not familiar
with type inference. It is abstracted away from the user but the (>>=) or bind
function is really a function of 3 arguments with the extra typeclass dictionary
argument ($dMonad) implicitly threaded around.

    main $dMonad =
        bind
            $dMonad
            getLine
            (\x ->
                bind
                    $dMonad
                    putStrLn (\_ -> return $dMonad ())
            )

-}

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

-- Examples of Monads provided OOTB
-- 1. Maybe
example1 :: Maybe Int
example1 = do
  a <- Just 3
  b <- Just 4
  return $ a + b
-- Just 7

example2 :: Maybe Int
example2 = do
  a <- Just 3
  b <- Nothing
  return $ a + b
-- 2. List
-- The list comprehension syntax in Haskell can be implemented in terms of the list monad.

example3 :: [(Int, Int)]
example3 = do
  a <- [1,2]
  b <- [10,20]
  return (a,b)

example4 :: [(Int, Int)]
example4 =
  [1,2] >>= \a ->
    [10,20] >>= \b ->
      return (a,b)

example5 :: [(Int, Int)]
example5 =
  concat (map (\a -> concat (map (\b -> return (a,b)) [10,20])) [1,2])

t2 = example5 == example4 && example4 == example3
-- 3. IO

-- Whats the point of Monads?
-----------------------------
{-
With Monads and defns for Maybe, List, IO, we now have a uniform interface
for talking about three very different but foundational ideas for programming:
Failure, Collections, and Effects.

-- analogous to list cons operator
mcons :: Monad m => m t -> m [t] -> m [t]
mcons p q   = do
    x <- p
    y <- q
    return (x:y)

-- sequence a list of monads using mcons above
sequence :: Monad m => [m a] -> m [a]
sequence    = foldr mcons (return [])
-}

seq_maybe   = sequence [Just 3, Just 4, Nothing]
seq_list    = sequence [[1,2,3],[10,20,30]]
seq_io      = sequence [getLine, getLine]

{-
So there we have it, three fundamental concepts of computation that are
normally defined independently of each other actually all share this
similar structure that can be abstracted out and reused to build higher
abstractions that work for all current and future implementations.

If you want a motivating reason for understanding monads, this is it!

This is the essence of what I wish I knew about monads looking back.
-}

-- More Examples of Monads provided OOTB
-- 4. Reader
{-
access shared immutable state from within a monadic context

Methods:
ask :: Reader a a
asks :: (r -> a) -> Reader r a
local :: (r -> b) -> Reader b a -> Reader r a
runReader :: Reader r a -> r -> a
-}
data MyContext = MyContext {
                    foo :: String,
                    bar :: Int
                } deriving Show
computation :: Reader MyContext (Maybe String)
computation = do
    n <- asks bar
    x <- asks foo
    if (n > 0)
        then return (Just x)
        else return Nothing

comp_1 = runReader computation $ MyContext "hello" 1
comp_2 = runReader computation $ MyContext "haskell" 0

-- 5. Writer
{-
emit a lazy stream of values from within a monadic context

Methods:
tell :: w -> Writer w ()
execWriter :: Writer w a -> w
runWriter :: Writer w a -> (a, w)


-}
type MyWriter = Writer [Int] String
twriter :: MyWriter
twriter = do
    tell [1..5]
    tell [5..10]
    return "foo"

example6 = runWriter twriter

-- 6. State
{-
allows functions within a stateful monadic context to access
and modify shared state

Methods:
runState  :: State s a -> s -> (a, s)
evalState :: State s a -> s -> a
execState :: State s a -> s -> s

get :: State s s
put :: s -> State s ()
modify :: (s -> s) -> State s ()

NOTE:
The state monad is often mistakenly described as being impure,
but it is in fact entirely pure and the same effect could be
achieved by explicitly passing state.
-}
testState :: State Int Int
testState = do
    put 3
    modify (+1)
    get
example7 = execState testState 0

