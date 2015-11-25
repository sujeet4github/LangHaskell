import Test.QuickCheck

-- The Monad type class
{-

The Monad class defines two basic functions, >>= (bind) and return

infixl 1 >>, >>=
class Monad m where
    (>>=)       :: m a -> (a -> m b) -> m b
    (>>)        :: m a -> m b -> m b
    return      :: a -> m a
    fail        :: String -> m a

    // default method
    m >> k      = m >>= \_ -> k
    // defined in terms of bind, discards its argument


The bind operations (>>= and >>) combine two monadic values
The return operation injects a value into the monad (container)

The do syntax provides a sugared form,
do { a <- f ; m } = f >>= \a -> do { m }
do { f ; m } = f >> do { m }
do { m } = m
with error conditions, the latter is kind of like:
do p <- e1; e2  = e1 >>= (\v -> case v of p -> e2; _ -> fail "s")
where s is the string identifying the location of the do statement for possible
use in an error message.
e.g. 'a' <- getChar will throw fail if the char typed is not 'a'
Errors in the list monad return a "zero" list

Monad laws for >>= and return are:
    return a >>= f      == f a
    m >>= return        == m
    xs >>= return . f   == fmap f . xs
    (m >>= f) >>= g)
        == m >>= (\x -> f x >>= g)

OR
#1
    do x <- m
        return x
    == do m
#2
    do
        y <- return x
        f x
    == do f x
#3
    do
        b <- do
                a <- m
                f a
        g b
    == do
        a <- m
        b <- f a
        g b
    == do
        a <- m
        do
            b <- f a
            g b

these laws are not enforced by Haskell
-}


-- The Prelude contains a 3 classes defining monads are they are used in Haskell.
-- IO
-- lists ([])
-- Maybe

prop_verify_ThreeWaysOfWritingAMonadicEquation min max =
    let
        -- List comprehension
        xListCompr = [(x,y) | x <- [min..max] , y <- [min..max], x /= y]
        -- do monadic sequence
        xDo = do
                x <- [min..max]
                y <- [min..max]
                True <- return (x /= y)
                return (x, y)
        -- raw monadic bind
        xRaw = [min..max] >>=
            (\ x -> [min..max] >>=
                (\ y -> return (x /= y) >>=
                    (\ r -> if (r == True)
                            then return (x,y)
                            else fail "")))
    in
        xListCompr == xDo
        && xRaw == xDo



{-
Why Monads?
==========
(1)
Explaining the monadic operators and their associated laws doesn't really show what monads are
good for. What they really provide is modularity. That is, by defining an operation monadically,
we can hide underlying machinery in a way that allows new features to be incorporated into the
monad transparently
(See- Wadler's Paper - "Monads for Functional Programming" In "Advanced Functional Programming"

(2)
Meanwhile "doing IO" requires hauling around file handles and their states and dealing with IO errors.
"Parsing" requires to track position in the input and dealing with parsing errors
In both cases Wise Men Who Wrote Libraries cared for our needs and hide all underlying complexities from us,
exposing the API of their libraries (IO and parsing) in the form of "monadic action" which we are free to
combine as we see fit.

-}

{-
This example defines a new type, SM, to be a computation that implicitly carries a type S.
That is, a computation of type SM t defines a value of type t while also interacting with
(reading and writing) the state of type S.

SM consists of functions that takes a state and produces 2 results
- a returned value (of any type)
- an updated state
-}

-- A State Monad built around a state type S:
type S = String
data SM a = SM (S -> (a, S)) -- the monadic type

{-
The instance declaration defines the plumbing of the monad:
- how to sequence two computations
- defn of an empty computation
-}
instance Monad SM where
    -- defines state propagation
    --  computations - c1 and c2
    --  initial state s0
    -- step 1: passes initial state s0 into computation s1
    --         result is (r, s1) where s1 is new state
    -- step 2: result from above (r) is passed to a function fc2
    --         that returns the second computation c2
    -- step 3: the new state s1 is passed to second compuation c2,
    --         and this result is the overall result
    --
    SM c1 >>= fc2       = SM (\ s0 -> let
                                            (r, s1) = c1 s0
                                            SM c2 = fc2 r
                                        in c2 s1)
    -- brings a value into the monad
    return k            = SM (\ s -> (k, s))

-- Monadic Primitive #1
-- extract state from the monad
readSM              :: SM S
readSM              = SM (\ s -> (s, s))

-- Monadic Primitive #2
-- update state of the monad
updateSM            :: (S -> S) -> SM ()
updateSM f          = SM (\ s -> ((), f s))

-- run a computation in the SM monad
runSM               :: S -> SM a -> (a, S)
runSM s0 (SM c)     = c s0

{-
The bigger picture:
we are trying to define an overall computation as a series
of steps (functions with type SM a) sequenced with >>= and return

these steps may interact with state (via readSM and updateSM)
The use (or non-use) of state is hidden - we do not invoke or
sequence our computations differently depending on whether or
not they use S
-}



{-
define a data type to denote a computation using resources controlled
by a monad.
Each computation is a function from available resources to remaining resources, coupled with either
a result, of type a, or a suspended computation, of type R a, capturing the work done up to the
point where resources were exhausted.
-}
data R a = R (Resource -> (Resource, Either a (R a)))

instance Monad R where
    -- two combine two 'resourceful' computations - c1 and fc2 (a function producing c2)
    -- Step 1: pass initial resources (r) into c1
    --         result will be either
    --         (1) - a value v, and remaining resources (r') which are used to determine
    --               next computation
    --         or (2) - a suspended computation (pc1) and resources remaining at point of
    --                  suspension
    --
    R c1 >>= fc2        = R (\ r -> case c1 r of
                                        (r', Left v)    -> let R c2 = fc2 v
                                                                in c2 r'
                                        (r', Right pc1) -> (r', Right (pc1 >>= fc2))
                                )

    return v            = R (\ r -> (r, (Left v)))

type Resource = Integer
