-- http://learnyouahaskell.com/for-a-few-monads-more#reader
--
{-
we saw that the function type, (->) r is an instance of Functor.
 Mapping a function f over a function g will make a function that
 takes the same thing as g, applies g to it and then applies f to that result.
-}
functions_are_functors = let
  f = (*5)
  g = (+3)
  in (fmap f g) 8

-- functions can be considered a value with a context.
-- the context is that the value is not yet present

-- functions also are applicative functors.
-- they allow us to operate on the eventual results of functions as if
-- we already had their results
functions_are_applicatives = let
  f = (+) <$> (*2) <*> (+10)
  in f 3

functions_are_monads = addStuff 3
  where
    addStuff = do
                a <- (*2)
                b <- (+10)
                return (a+b)

{-
    >>= for functions is implemented as,
    h >>= f = \w -> f (h w) w
-}


-- In a reader, all the functions read from a common source
