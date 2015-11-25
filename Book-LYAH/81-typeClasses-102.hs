-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#typeclasses-102
--

{-

A quick recap on typeclasses: typeclasses are like interfaces.
A typeclass defines some behavior (like comparing for equality, comparing for ordering,
enumeration) and then types that can behave in that way are made instances of that typeclass.

The behavior of typeclasses is achieved by defining functions or just type declarations
that we then implement. So when we say that a type is an instance of a typeclass, we mean
that we can use the functions that the typeclass defines with that type.


class Eq a where ...
 this means that we're defining a new typeclass and that's called Eq.
The a is the type variable and it means that a will play the role of the type that we
will soon be making an instance of Eq. It doesn't have to be called a, it doesn't even
have to be one letter, it just has to be a lowercase word...but we usually opt for single letters to be true to the Haskell style.

Then, we define several functions. It's not mandatory to implement the function bodies
themselves, we just have to specify the type declarations for the functions.

Def: Minimal complete definition for the typeclass — the minimum of functions that we have
to implement so that our type can behave like the class advertises

You can also make typeclasses that are subclasses of other typeclasses.
class (Eq a) => Num a where ...
-}
import Prelude hiding (Eq, (==), (/=))

data TrafficLight = Red | Yellow | Green
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    -- it was not necessary to define the function bodies, but they were given as a convinence
    -- so we just have to define one in the actual instance and the other would follow
    x == y = not (x /= y)
    x /= y = not (x == y)

instance Eq TrafficLight
    -- not defining == or /= results in infinite loop on using it...


instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"


-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass
{-
    The functor type class is for things that can be mapped over
    e.g. lists, list type is part of the Functor typeclass
    the type of fmap is interesting:

class Functor f where
    fmap :: (a -> b) -> f a -> f b

    But now, the f is not a concrete type (a type that a value can hold, like Int, Bool or Maybe String),
    but a type constructor that takes one type parameter. i.e. it has kind * -> *
    e.g. Maybe, [] etc

    fmap takes a f from one type and returns a f of another type.
    e.g. map is a fmap that works only on lists


-- functor for Either - remember by convention Left is error, Right is success value
-- we need to make
instance Functor (Either a) where
    fmap f (Left x) = Left x
    fmap f (Right x) = Right (f x)

    -}
