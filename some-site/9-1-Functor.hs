
{-
Functor class (taken from the Prelude):

class Functor f where
	fmap :: (a -> b) -> f a -> f b

The map function applies an operation to the objects inside a container
(polymorphic types can be thought of as containers for values of another type),
returning a container of the same shape.

The following laws apply to fmap:
fmap id		= id
fmap (f . g)= fmap f . fmap g

These laws ensure that the container shape is unchanged by fmap and the contents
of the container are not rearranged by the map operation.

-}

-- from 2-userDefined...hs
--
data Tree a		= Leaf a | Branch (Tree a) (Tree a) deriving Eq

-- The instance declaration declares that Tree (higher order), rather than Tree a (first order), is an instance of Functor
--
instance Functor Tree where
	fmap f (Leaf x) = Leaf (f x)
	fmap f (Branch t1 t2) = Branch (fmap f t1) (fmap f t2)
