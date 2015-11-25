import Test.QuickCheck

-- use: data for creating new types
-- ================================

-- enumerated types
-- note the 3 constructors Red, Green and Blue
data Color	= Red | Green | Blue

data Point a = Pt a a a deriving Show
{-
syntax: data <name-of-type> [type-expressions] = <constructor> <value-expression> [deriving ....]
	by convention constructor uses same name as type, so the RHS should be written as Point a a a
There are three "member" variables in data-type Point hence the three a's
	:t Pt gives us the following type for the constructor, Pt :: a -> a -> a -> Point a
-}

{-
Constructors in a data declaration may be declared with associated field names, enclosed in
braces. These field names identify the components of constructor by name rather than by position.
-}
-- This is an alternative way to define Point:
data Point' = Pt' {pointx, pointy, pointz :: Float} deriving Show
{-
These field names can be used as selector functions to extract a component from a structure.
In this example, the selectors are:
pointx :: Point -> Float
pointy :: Point -> Float
-}
absPoint	:: Point' -> Float
absPoint p = sqrt (pointx p * pointx p + pointy p * pointy p)
-- another way to define absPoint using pattern matching on constructor
absPoint' (Pt' {pointx = x, pointy = y}) = sqrt (x*x + y*y)

prop_verifyAbsPoint_specific = 10.630146 == (let p = Pt' 8.0 7.0 5.0 in absPoint p)
prop_verifyAbsPoint_specific2 = (let p = Pt' 8.0 7.0 5.0 in absPoint' p) == (let p = Pt' 8.0 7.0 5.0 in absPoint p)
prop_verifyAbsPoint x y z = (x*x + y*y) == (let p = Pt' x y z in absPoint p) && (let p = Pt' x y z in absPoint' p) == (let p = Pt' x y z in absPoint p)

-- type constructor vs value or data constructor
-- type expression vs value expression

-- predefined Show helps with conversion to strings - for printing etc


-- types can be recursive - e.g. Trees
data Tree a		= Leaf a | Branch (Tree a) (Tree a)
{-
here, Tree is a type constructor
and, Leaf and Branch are value constructors

we have defined a polymorphic binary tree type whose elements are either leaf nodes containing
a value of type a, or internal nodes (branches) containing (recursively) two sub-trees.
-}

-- function to map a Tree to a List
fringe	:: Tree a -> [a]
fringe (Leaf v)	= [v]
fringe (Branch l r) = fringe l ++ fringe r

-- use type for synonyms
-- =====================
type Person = (Name, Address)
type Name = String
data Address = None | Addr String

-- use newtype for creating new types from existing
-- ================================================
newtype Natural = MakeNatural Integer deriving Show
-- the constructor MakeNatural takes a single Integer
--   use this constructor to pattern match to convert Natural to Integer
toNatural		:: Integer -> Natural
toNatural	x | x < 0	= error "Can't create negative naturals!"
			  | otherwise = MakeNatural x
fromNatural		:: Natural -> Integer
fromNatural (MakeNatural i) = i

-- the following instance declaration admits Natural to the Num class
instance Num Natural where
	fromInteger		= toNatural
	x + y			= toNatural (fromNatural x + fromNatural y)
	x - y			= let r = fromNatural x - fromNatural y in
						if r < 0 then error "Unnatural subtraction"
								 else toNatural r
	x * y			= toNatural (fromNatural x * fromNatural y)



-- Strict Data Constructors
-- ========================
{-
Data structures in Haskell are generally lazy: the components are not evaluated until needed.
Internally, each field of a lazy data object is wrapped up in a structure commonly referred to
as a thunk that encapsulates the computation defining the field value.
This thunk is not entered until the value is needed - thunks that have errors do not affect the
other elements of a data structure.
There are a number of overheads associated with thunks: they take time to construct and
evaluate, they occupy space in the heap, and they cause the garbage collector to retain other
structures needed for the evaluation of the thunk.
Strictness (!) fields in a data declarations allow specific fields in a constructor to be
evaluated immediately, selectively suppressing laziness.
A field marked by ! in a data declaration is evaluated when the structure is created instead
of delayed in a thunk.

There are a number of situations where it may be appropriate to use strictness flags:
- Structure components that are sure to be evaluated at some point during program execution.
- Structure components that are simple to evaluate and never cause errors.
- Types in which partially undefined values are not meaningful.
-}

-- Note: The :+ marks the constructor as infix