-- http://virtuslab.com/blog/arrows-monads-and-kleisli-part-i/
--

{-
Arrow:
    It was called a general interface to computation and it looked like a technique
    that could bridge code that is imperative in nature with proper functional programming.

    Railway Oriented Programming
-}

{-
(Wiki)
Arrows are a type class used in programming to describe computations in a pure and
declarative fashion.
First proposed by computer scientist John Hughes as a generalization of monads,
arrows provide a referentially transparent way of expressing relationships between
logical steps in a computation.
Unlike monads, arrows don’t limit steps to having one and only one input.

(Haskell Wiki)
Arrow a b c represents a process that takes as input something of type b and
outputs something of type c.

You can treat arrow as a function type that gives you more combinators to work
with than function’s compose or andThen.
Composing arrows is not only more expressive but can encapsulate rich semantics
e.g. you can compose monadic processes.


-}
import Prelude hiding (id)

class Category cat where
    -- the identity morphism
    id :: cat a a
    -- composition morphism
    (.) :: cat b c -> cat a b -> cat a c -- infixr 9
-- helper methods
-- right to left composition
-- (<<<) :: Category cat => cat b c -> cat a b -> cat a c -- infixr 1
-- left to right composition
-- (>>>) :: Category cat => cat a b -> cat b c -> cat a c -- infixr 1
--
-- composition operators >>>, <<< that can attaches a second category to a first
-- as long as the first function’s output and the second’s input have matching types
--
class Category a => Arrow a where
    -- lift a function to an arrow (a here is the arrow)
    arr :: (b -> c) -> a b c
    -- send the first component of the input through the argument arrow,
    -- copy the rest unchanged to the output
    -- Note: a is the arrow and the function is (b -> c)
    first :: a b c -> a (b, d) (c, d)
    second :: a b c -> a (d, b) (d, c)
    -- Split the input between the two argument arrows and combine their output.
    -- Note that this is in general not a functor.
    (***) :: a b c -> a b' c' -> a (b, b') (c, c') -- infixr 3
    -- merging operator *** that can take two arrows, possibly with different input
    -- and output types, and fuse them into one arrow between two compound types.
    -- a s t *** a u v -> a (s,u) (t,v)

    -- Fanout: send the input to both argument arrows and combine their output.
    (&&&) :: a b c -> a b c' -> a b (c, c') -- infixr 3
    -- split operator &&&, sends the input to both argument arrows and combines
    -- their output.
    -- a s t &&& a s u -> a s (t, u).

{-
arrow_law_1 :: Bool
arrow_law_1 = arr id == id

arrow_law_2 = arr (f >>> g) = arr f >>> arr g

arrow_law_3 = first (arr f) = arr (first f)

arrow_law_4 = first (f >>> g) = first f >>> first g

arrow_law_5 = first f >>> arr fst = arr fst >>> f

arrow_law_6 = first f >>> arr (id *** g) = arr (id *** g) >>> first f

arrow_law_7 = first (first f) >>> arr assoc = arr assoc >>> first f

-- where
assoc ((a, b), c) = (a, (b, c))

-}

{-
called Kleisli to honor Swiss mathematician who studied properties of categories associated to monads.

-}