-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types
--
-- See Shapes.hs

-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#record-syntax
--
-- See Person.hs
-- See Car.hs

-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#type-parameters
--
-- see Vector.hs

{-
    A value constructor can take some values parameters and then produce a new value.

    type constructors can take types as parameters to produce new types.

    Using type parameters is very beneficial, but only when using them makes sense.
    Usually we use them when our data type would work regardless of the type of the
    value it then holds inside it, like with our Maybe a type.
    If our type acts as some kind of box, it's good to use them.
-}

-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#derived-instances
--
{-
A typeclass is a sort of an interface that defines some behavior.
A type can be made an instance of a typeclass if it supports that behavior.
Example: the Int type is an instance of the Eq typeclass because the Eq typeclass defines
behavior for stuff that can be equated. And because integers can be equated, Int is a part
of the Eq typeclass.

Ord type class is for types that have values that can be ordered.
    If we compare two values of the same type that were made using different constructors,
    the value which was made with a constructor that's defined first is considered smaller

Enum typeclass is for things that have predecessors and successors
Bounded typeclass is for things that have a lowest possible value and highest possible value
-}
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#type-synonyms\
--
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

test_phoneBook = inPhoneBook "patsy" "493-2928" phoneBook

-- Recursive Data Structures
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#recursive-data-structures

data List1 a = EmptyList1
                | Cons { listHead :: a, listTail :: List1 a }
                deriving (Show)
-- define functions to be automatically infix by making them comprised of only special characters.
-- We can also do the same with constructors, since they're just functions that return a data type.
data List2 a = EmptyList2
                | a :-: List2 a
                deriving (Show)
{-
    a new syntactic construct, the fixity declaration.
    When we define functions as operators, we can use that to give them a fixity (but we don't have to).
    A fixity states how tightly the operator binds and whether it's left-associative or right-associative.
    For instance, *'s fixity is infixl 7 * and +'s fixity is infixl 6.
    That means that they're both left-associative (4 * 3 * 2 is (4 * 3) * 2) but * binds tighter than +,
    because it has a greater fixity, so 5 * 4 + 3 is (5 * 4) + 3.
-}
infixr 5 :-:
a = 3 :-: 4 :-: 5 :-: EmptyList2



data BTree a = EmptyTree
                | Node a (BTree a) (BTree a)
                deriving (Show)

singleton :: a -> BTree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> BTree a -> BTree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
            | x == y    = Node y left right
            | x < y     = Node y (treeInsert x left) right
            | otherwise = Node y left (treeInsert x right)

treeElem :: (Ord a) => a -> BTree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right)
            | x == y    = True
            | x < y     = treeElem x left
            | otherwise = treeElem x right
