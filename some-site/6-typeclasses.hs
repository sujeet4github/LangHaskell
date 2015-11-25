{-
type classes provide a structured way to control overloading
-}

{-
Here Eq is the name of the class being defined, and == is the single operation in the class.

This declaration may be read "a type a is an instance of the class Eq if there is an (overloaded) operation
==, of the appropriate type, defined on it."
(Note that == is only defined on pairs of objects of the same type. (the type variable a)

The constraint that a type a must be an instance of the class Eq is written Eq a (before =>)
This type constraint is also called Context
NOTE: Eq has been defined already in Prelude with ==, avoiding errors using the ' and the extra = in ===
-}
class Eq' a where
	-- For every type a that is an instance of the class Eq, == has type a->a->Bool
	(===) :: (Eq' a) => a -> a -> Bool

-- Using above class declarations
elem' :: (Eq' a) => a -> [a] -> Bool
x `elem'` []		= False
x `elem'` (y:ys)	= x === y || (x `elem'` ys)

-- Note: === has been declared in class Eq`, the actual behavior if different for differnt types of a
-- will be overloaded using an instance declaration
instance Eq' Integer where
	x === y		= x `integerEq` y
instance Eq' Float where
	x === y		= x `floatEq` y

integerEq :: (Integer i) => i -> i -> Bool
ll `integerEq` rr	= ll == rr

floatEq :: (Float f) => f -> f -> Bool
l `floatEq` r	= l == r

-- load 2-userDefinedAndPolymorphicTypes.hs to get Tree for this
data Tree a		= Leaf a | Branch (Tree a) (Tree a)
-- Note: the context Eq' a in the instance declaration. this is necessary because the elements in the leaves
-- are being compared using ===
--
instance (Eq' t) => Eq'(Tree t) where
	Leaf a === Leaf b 			= a === b
	(Branch l1 r1) === (Branch l2 r2)	= (l1===l2) && (r1===r2)
	_ === _					= False


{-
The Haskell Report, especially the Prelude, contains a wealth of useful examples of type classes.
Indeed, a class Eq is defined that is slightly larger than the one defined earlier:

class Eq a where
	(==), (/=) :: a -> a -> Bool
	-- demonstrates a default implementation
	x /= y = not (x == y)

class (Eq a) => Ord a where
	(<), (<=), (>=), (>) :: a -> a -> Bool
	max, min :: a -> a -> a

Haskell also permits multiple inheritance, since classes may have more than one superclass. For
example, the declaration

class (Eq a, Show a) => C a where ...

creates a class C which inherits operations from both Eq and Show.

Eq Integer - First Order
Eq - Higher Order
-}


{-
Each type has an associated kind which ensures that the type is used correctly.

Type expressions are classified into different kinds which take one of two possible forms:
- The symbol * represents the kind of type associated with concrete data objects.
  That is, if the value v has type t , the kind of v must be *.
- If k1 and k2 are kinds, then k1 -> k2 is the kind of types that take a type of kind k1 and return a type of kind k2.

The compiler infers kinds before doing type checking without any need for `kind declarations'.
Kinds stay in the background of a Haskell program except when an erroneous type signature leads to a kind error.
Kinds are simple enough that compilers should be able to provide descriptive error messages when kind conflicts occur.
-}

{-
-}
