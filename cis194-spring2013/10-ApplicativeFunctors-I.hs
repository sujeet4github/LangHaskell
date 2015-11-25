-- Applicative Functors Part I
-- http://www.seas.upenn.edu/~cis194/spring13/lectures/10-applicative.html

import Prelude hiding (Applicative, (<*>))
-- Suggested Reading
-- LYAH - http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors
-- http://www.haskell.org/haskellwiki/Typeclassopedia
--

-- Motivation:
-- Consider the following Employee type:
type Name = String
type PhoneNumber = String
data Employee = Employee { name :: Name,
                           phone :: PhoneNumber}
                           deriving (Show)

-- If we have a Name and a PhoneNumber we can apply the
-- Employee constructor to build an Employee object.

-- Suppose we only have a Maybe Name and a Maybe PhoneNumber
-- Perhaps they came from parsing some file full of errors,
--  or from a form where some of the fields might have been left blank,
--  or something of that sort.
--
-- we cant build an Employee, but we could build a Maybe Employee

-- Similarly, can we build a [Employee] from a [Name] and a [PhoneNumber]

-- or how about this:
--   we have an (e -> Name) and (e -> PhoneNumber) for some type e.
--   perhaps e is some huge data structure, and we have functions telling us
--   how to extract a Name and a PhoneNumber from it.
-- can we make it into a (e -> Employee), that is, a recipe for extracting
-- an Employee from the same structure
--

{-
Generalizing the above 3:
The type of the function we need is

xyz :: (a -> b -> c) -> (f a -> f b -> f c)

This looks very similar to:
fmap :: (a -> b) -> (f a -> f b)
However, functor does not give us enough to implement xyz:

We have:
fa :: f a
fb :: f b
h :: a -> b -> c
which can also be written as:
h :: a -> (b -> c)
fmap h :: f a -> f (b -> c)
fmap h fa :: f (b -> c)

OK, so now we have something of type f (b -> c) and something of type f b…
and here’s where we are stuck! fmap does not help any more.

Functor - fmap gives us a way to apply functions to values inside a
Functor context.

We need now to apply a function (which is itself in a Functor context) to
values in a Functor context.

Functors for which this sort of “contextual application” is possible are called
applicative, and the Applicative class captures this pattern.
-}

-- defined in Control.Applicative
class Functor f => Applicative f where
  -- lets us inject a value of type a into a container.
  pure  :: a -> f a

  -- pronounced ap - short for apply
  (<*>) :: f (a -> b) -> f a -> f b
--
-- Note also that the Applicative class requires its instances to be instances
-- of Functor as well, so we can always use fmap with instances of Applicative.
xyz :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
xyz h fa fb = (h `fmap` fa) <*> fb
-- Control.Applicative defines (<$>) as a synonym for fmap,
xyz' :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
xyz' h fa fb = (h <$> fa) <*> fb

-- In the standard library xyz is actually called liftA2:
-- similarly there is liftA3:
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 h fa fb fc = (h <$> fa) <*> fb <*> fc

-- Unlike the jump from fmap to liftA2 (which required generalizing from
-- Functor to Applicative), going from liftA2 to liftA3 (and from there to
-- liftA4, …) requires no extra power—Applicative is enough.


-- But what about pure? pure is for situations where we want to apply some
-- function to arguments in the context of some functor f, but one or more
-- of the arguments is not in f—those arguments are “pure”, so to speak.
-- We can use pure to lift them up into f first before applying.
-- Like so:
liftX :: Applicative f => (a -> b -> c -> d) -> f a -> b -> f c -> f d
liftX h fa b fc = (h <$> fa) <*> Main.pure b <*> fc

{-
Applicative Laws:
Really only one "interesting" one
f `fmap` x === pure f <*> x

Mapping a function f over a container x ought to give the
same result as first injecting the function into the container
and then applying it to x with <*>

There are other laws...
-}

-- Applicative examples
instance Applicative Maybe where
  pure              = Just
  Nothing <*> _     = Nothing
  _ <*> Nothing     = Nothing
  Just f <*> Just x = Just (f x)
