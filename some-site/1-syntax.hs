import Test.QuickCheck

---------------------------------------------------------------------------------------------------------------------
-- Declarations
-- ============

-- :: is read as "has type"

{-
 Haskell's type system is powerful enough to allow us to avoid writing any type signatures at all
 we say that the type system infers the correct types for us. Nevertheless, judicious placement of type
 signatures such as that we gave for inc is a good idea, since type signatures are a very effective
 form of documentation and help bring programming errors to light.
-}

-- types are Capitalized, indentifiers for values are not - this is enforced by the Haskell lexical syntax

-- Function Declarations
-- ---------------------
-- Conventions to define a function
-- first type signature declaration
inc :: Integer -> Integer
-- next defn equation
inc n = n + 1
-- alternate way - called fixed point?
inc' = (+) 1

-- Class Declarations
-- ------------------
-- key words - class, where
class JJ t where
	(===)	:: (JJ t) => t -> t -> Bool

-- Instance Declarations
-- ---------------------
-- key words - instance, where
instance JJ Integer where
	x === y		= x `integerEq` y

---------------------------------------------------------------------------------------------------------------------
-- Lists
-- =====
l = [1, 2, 3]
-- [] notation is syntactic sugar for
l1 = 1:(2:(3:[]))
-- operator : is right associative
l2 = 1:2:3:[]
-- Strings notations "" is syntactic sugar
"abc" = 'a':'b':'c':[]

-- For convenience and by tradition, all QuickCheck tests begin with prefix "prop_".
prop_list_notations =
      	l == l1 &&
      	l1 == l2 &&
      	"abc" == 'a':'b':'c':[]

-- zip is a Standard Prelude function that returns the pairwise interleaving of its two list
-- arguments, defined as :

zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' xs ys = []
prop_zip_works_simple = zipEval == zip [1, 2, 3, 4] ['a', 'b'] &&
				zipEval == [(1, 'a'), (2, 'b')]
					where zipEval = zip' [1, 2, 3, 4] ['a', 'b']
prop_zip_works xs ys =
	zip xs ys == zip' xs ys

---------------------------------------------------------------------------------------------------------------------
-- User Defined Function on List
-- =============================
-- polymorphic types (with type variables) <==> generics in java. Parametric Polymorphism (vs Ad hod polymorphism
-- using type classes in 6-typeclasses.hs)
-- e.g the len function below works on a list of any type - 'generic' type variable a is used, thus len is a polymorphic function
len		:: [a] -> Integer
len	[]		= 0
len	(x:xs)	= 1 + len xs
-- function is defined above using pattern matching - avoids if-else statements

-- For convenience and by tradition, all QuickCheck tests begin with prefix "prop_".
-- Assume that "xs" will be a random list and code your test.
prop_test_len_function xs =
  let l = len xs
      in fromInteger l == length xs

---------------------------------------------------------------------------------------------------------------------
-- Arithmetic Sequences or Ranges
-- ==============================
-- tries to be smart :-)
-- [1,3..10] == [1, 3, 5, 7, 9]
prop_seq_notations =
	[1,3..10] == [1, 3, 5, 7, 9]

-- infinite lists from ranges
-- [2..]
-- [1, 4..] -- as in 1st example second element specifies distance between elements
--
prop_seq_notations_for_chars = ['a'..'z'] === "abcdefghijklmnopqrstuvwxyz"

---------------------------------------------------------------------------------------------------------------------
-- Operators
-- =========
-- function composition operator - .
(.)		::	(b->c) -> (a->b) -> (a->c)
f . g	= \ x -> f ( g ( x ) )
-- Anonymous function can be made using lambda expressions. For example
--
--	\x -> x + 1
--
-- is a function with one parameter which it will add one to. Lambda expressions prove useful as arguments to for example map:
--	map (\xs -> zip xs [1..]) list
--
-- In general:
--	\pattern1 pattern2 ... patternn -> expression		(n>=1)

---------------------------------------------------------------------------------------------------------------------
-- Pattern Matching, Case Expressions, Lazy Patterns
-- =================================================
-- in 4-functions.hs
-- Conditional Expression
-- if e1 then e2 else e3
-- This is syntactic sugar for:
-- case e1 of True -> e2
--            False -> e3


---------------------------------------------------------------------------------------------------------------------
{-
Symbols, Keywords
=================

=

::
	read as - has type

->
	For functions, (->) is a type constructor; the types f -> g and (->) f g are the same.

<-
	extract value from an IO

[]
	used for lists. Note [a] and ([] a) are equivalent
	.. within [] is for arithmetic sequences/ranges

()
	used for tuples. (1, 'a')
	() is unit type - similar to void

{}
	braces - optional - see Layout
	       - mandatory for field labels in type declarations

!
	strictness flag, in data declaration only

$
	function call, with lowest precedence

.
	function composition

class
instance
where
let
	expressions
	in do to define regular bindings
data
newtype
type
case

-}
