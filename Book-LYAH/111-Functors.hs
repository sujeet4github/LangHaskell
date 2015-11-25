-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids#functors-redux
--

{-
    continuation of Functors from 81-typeClasses-102.hs

    Many times the box analogy is used to help you get some intuition for how
    functors work, however for some functors, the box analogy has to be stretched
    really thin...
    A more correct term would be Computational Context

    to make a type constructor an instance of Functor, it has to have a kind of * -> *,
    which means that it has to take exactly one concrete type as a type parameter.

    if we write fmap :: (a -> b) -> (f a -> f b), we can think of fmap not as a
    function that takes one function and a functor and returns a functor, but as
    a function that takes a function and returns a new function that's just like
    the old one, only it takes a functor as a parameter and returns a functor as
    the result.
    It takes an a -> b function and returns a function f a -> f b. This is called
    lifting a function.


-}

-- Functor Law #1
--  fmap id == id
test_f_law_1 = let
  works_on_just = fmap id (Just 3) == Just 3
  works_on_list = fmap id [1..5] == [1..5]
  in
    works_on_just
    && works_on_list

-- Functor Law #2
--  composing two functions and then mapping the resulting function over a functor
--  should be the same as first mapping one function over the functor and then
--  mapping the other one.
-- for any functor F, the following should hold:
-- fmap (f . g) F = fmap f (fmap g F)

{-
  At first, the functor laws might seem a bit confusing and unnecessary, but then
   we see that if we know that a type obeys both laws, we can make certain
   assumptions about how it will act. If a type obeys the functor laws, we know
   that calling fmap on a value of that type will only map the function over it,
   nothing more. This leads to code that is more abstract and extensible, because
   we can use laws to reason about behaviors that any functor should have and
   make functions that operate reliably on any functor.

-}
