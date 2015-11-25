-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors
--

{-
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

the first line starts the definition of the Applicative class and it also
introduces a class constraint.
It says that if we want to make a type constructor part of the Applicative
typeclass, it has to be in Functor first. That's why if we know that if a
type constructor is part of the Applicative typeclass, it's also in Functor,
so we can use fmap on it.

pure take a value and we wrap it in an applicative functor that has that value
as the result inside it. Use the box analogy Or better yet, pure takes a value
and puts it in some sort of default (or pure) context—a minimal context that
still yields that value.

<*> is a beefed up fmap:
<*> :: f (a -> b) -> f a -> f b vs fmap :: (a -> b) -> f a -> f b
fmap takes a function and a functor, and applies the function inside the functor.
<*> takes a functor with a function in it, and another functor, and sort of
extracts that function from the first functor and then maps it over the second one.
When I say extract, I actually sort of mean run and then extract, maybe even sequence.
We'll see why soon.

-- Applicative instance implementation for Maybe
--
-- f that plays the role of the applicative functor should take one concrete type
-- as a parameter, so we write instance Applicative Maybe where instead of writing
-- instance Applicative (Maybe a) where
instance Applicative Maybe where
    -- take something and wrap it in an applicative functor
    pure = Just
    -- We can't extract a function out of a Nothing, because it has no function
    -- inside it. So result is Nothing
    Nothing <*> _ = Nothing
    -- extracts the function from the left value if it's a Just and maps it over
    -- the right value.
    -- Note the f is a functor as per class constraint.
    -- If any of the parameters is Nothing, Nothing is the result.
    (Just f) <*> something = fmap f something

-}

-- utlity replacement for fmap
-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b
-- where,
-- f <$> x = fmap f x

-- Applicative Law #1
-- pure f <*> x == fmap f x
-- other laws:
-- pure id <*> v == v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- u <*> pure y = pure ($ y) <*> u

