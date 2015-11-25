-- Recursion patterns, polymorphism, and the Prelude
-- http://www.seas.upenn.edu/~cis194/spring13/lectures/03-rec-poly.html

-- map
-- filter
-- fold or reduce

-- Haskell supports polymorphism for both data types and functions

-- The Prelude is a module with a bunch of standard definitions that gets
-- implicitly imported into every Haskell program

-- Functions which are well-defined on all possible inputs are known as total functions
-- else partial functions - there are some inputs for which it will crash/recurse indefinitely
--
-- It is good Haskell practice to avoid partial functions as much as possible.
--
-- Prelude partial functions you should almost never use include head, tail, init, last, and (!!)

-- What if you find yourself writing a partial functions?
-- There are two approaches to take.
-- The first is to change the output type of the function to indicate the possible failure. (Maybe)
--   http://hackage.haskell.org/package/safe - safe package redefines some unsafe methods from Prelude
-- The second is to have the compiler guarantee the condition by using appropriate types
--   e.g head on a Non-Empty list - define a non-empty-list type and have compiler validate
data NonEmptyList a = NEL a [a]
  deriving Show

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as
