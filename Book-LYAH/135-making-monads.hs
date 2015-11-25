-- http://learnyouahaskell.com/for-a-few-monads-more#making-monads
--

{-
lists are used to represent non-deterministic values. A list like [3,5,9] can
be viewed as a single non-deterministic value that just can't decide what it's
going to be. When we feed a list into a function with >>=, it just makes all
the possible choices of taking an element from the list and applying the function
to it and then presents those results in a list as well.

If we look at the list [3,5,9] as the numbers 3, 5 and 9 occurring at once, we
might notice that there's no info regarding the probability that each of those
numbers occurs. What if we wanted to model a non-deterministic value like [3,5,9],
but we wanted to express that 3 has a 50% chance of happening and 5 and 9 both
have a 25% chance of happening? Let's try and make this happen!

In mathematics, probabilities aren't usually expressed in percentages, but rather
in real numbers between a 0 and 1. A 0 means that there's no chance in hell for
something to happen and a 1 means that it's happening for sure. Floating point
numbers can get real messy real fast because they tend to lose precision, so
Haskell offers us a data type for rational numbers that doesn't lose precision.
That type is called Rational and it lives in Data.Ratio.

-}

import Data.Ratio

l1 = [3,5,9]

l2 = [ (3,0.5),(5,0.25),(9,0.25) ]

l3 = [ (3,1%2),(5,1%4),(9,1%4) ]

-- We took lists and we added some extra context to them, so this represents
-- values withs contexts too. Before we go any further, let's wrap this into
-- a newtype because something tells me we'll be making some instances.

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show
-- Functor
instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs
fmap_test = fmap negate (Prob l3)

-- For monads
-- m >>= f == join (fmap f m)

thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )
    ,( Prob [('c',1%2),('d',1%2)] , 3%4)
    ]

NON COMPREHENDE!!!!