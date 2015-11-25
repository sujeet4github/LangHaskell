module Tree where

import qualified Data.Monoid as Mon

data Tree a = Empty
                | Node a (Tree a) (Tree a)
                deriving (Show, Read, Eq)

data Direction = L | R deriving Show
type Directions = [Direction]

-- we make it an instance of Functor and with that we gained the ability
-- to fmap functions over it.
instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- we make it an instance of Foldable so that we get the abilty to fold it up
-- One way to make a type constructor an instance of Foldable is to just directly
-- implement foldr for it. But another, often much easier way, is to implement
-- the foldMap function, which is also a part of the Foldable type class.
-- The foldMap function has the following type:
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
instance Foldable Tree where
    -- takes an instance of Tree and returns a Monoid
    foldMap f Empty = mempty
    foldMap f (Node x l r) = foldMap f l
                                `mappend` f x
                                `mappend` foldMap f r


-- we get foldl, foldr etc for free
testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )

addUp = foldl (+) 0 testTree
multUp = foldr (*) 1 testTree

-- if we want to know if any number in our tree is equal to 3
anyThrees = Mon.getAny $ foldMap (\x -> Mon.Any $ x == 3) testTree

-- another example tree
freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

elemAt :: Directions -> Tree a -> a
elemAt []     (Node x _ _)  = x
elemAt _      Empty         = error "Empty tree yields no content"
elemAt (L:ds) (Node _ l _)  = elemAt ds l
elemAt (R:ds) (Node _ _ r)  = elemAt ds r

-- see 136-zippers.hs
