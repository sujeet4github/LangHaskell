import Test.QuickCheck
-- for q/c tree generation
import Control.Monad (liftM2, replicateM)


{-

Show as defined:
Show - types that have... like toString
	show	:: (Show a) => a -> String
	show x	= shows x ""
	shows	:: (Show a) => a -> String -> String

let's consider a function to represent the binary trees of Section 2.2.1 as a string,
with suitable markings to show the nesting of subtrees and the separation of left and
right branches (provided the element type is representable as a string)

-}
data Tree' a		= Leaf a | Branch (Tree' a) (Tree' a) deriving Eq

showTree1 :: (Show a) => Tree' a -> String
showTree1 (Leaf x)		= show x
showTree1 (Branch l r)	= "<" ++ showTree1 l ++ "|" ++ showTree1 r ++ ">"

-- tests
prop_showTree1_leaf = showTree1 (Leaf 100) == "100"
prop_showTree1_branch = showTree1 (Branch (Leaf 10) (Leaf 100)) == "<10|100>"

{-
(++) has time complexity linear in the length of its left argument
	-> showTree1 is potentially quadratic in the size of the tree

method shows has an additional accumulator argument that can help with
 performance.
-}

-- type ShowS = String -> String

showsTree1 :: (Show a) => Tree' a -> ShowS
showsTree1 (Leaf x) s		= shows x s
showsTree1 (Branch l r) s	= '<' : showsTree1 l ('|' : showsTree1 r ('>' : s))

-- tests
prop_showsTree1_leaf = showsTree1 (Leaf 100) "" == "100"
prop_showsTree1_branch = showsTree1 (Branch (Leaf 10) (Leaf 100)) "" == "<10|100>"


-- TODO prove that shows version is faster than show
-- teach QuickCheck to generate a Tree'
{-
instance Arbitrary Tree' where
	arbitrary = (Tree' f) ++++ l
		where
			f = do
					return choose (1, 1024)
			l = do
					n <- choose (1, 1024)
					return [1..n]
-}

--prop_showsTree1_list xs = list2Tree xs

-- methods to build a tree
getVal :: Ord a => Tree' a -> a
getVal (Leaf v) = v
getVal (Branch l r) = getVal r

addTree :: Ord a => Maybe (Tree' a) -> a -> Tree' a
Nothing `addTree` v = (Leaf v)
Just (Leaf v) `addTree` v2 | v == v2	= Leaf v
Just (Leaf v) `addTree` v2 | v < v2	= Branch (Leaf v) (Leaf v2)
Just (Leaf v) `addTree` v2 | v > v2	= Branch (Leaf v2) (Leaf v)
Just (Branch l r) `addTree` v | v == getVal r = (Branch l r)
Just (Branch l r) `addTree` v | v > getVal r = Branch l ((Just r) `addTree` v)
Just (Branch l r) `addTree` v | v < getVal r = Branch ((Just l) `addTree` v) r

(+++) :: Ord a => Tree' a -> a -> Tree' a
t +++ v = (Just t) `addTree` v

-- list2Tree :: Ord a => [a] -> Tree' a
-- list2Tree (x:xs) = foldl (+++) (Leaf x) xs

{-
parser for type a (of typeclass Read). The function takes a string and a
list of pairs - [(a, String)]

type ReadS a = String -> [(a, String)]
reads	:: (Read a) -> ReadS a

-}

readsTree1	:: (Read a) => ReadS (Tree' a)
{-
1. The tree l can be parsed from the beginning of the string (after a '<')
2. The remaining string (after the representation of l) begins with '|', this is marked as t (w/o the '|')
3. The tree r can be parsed from the above string t at the beginning
4. The string that remains, starts with '>', this '>' is removed and u is the tail
-}
readsTree1 ('<':s)	= [(Branch l r, u) |	(l, '|':t) <- readsTree1 s,
											(r, '>':u) <- readsTree1 t]
-- to parse a Leaf, parse a representation of the element type of the tree
-- and apply the constructor Leaf to this value
readsTree1 s		= [(Leaf x, t) | (x,t) <- reads s]


{-
the parser reads is quite rigid - no white space etc.
also the way we parse punctuation symbols is different from parsing the leaf/subtrees
 - this makes the function defn harder to read

instead of parser read, use the lexical analyzer lex from Prelude

lex normally returns a singleton list containing a pair of strings:
	the first lexeme in the input string and the remainder of the input.
If the input string is empty or contains only whitespace and comments, lex returns [("","")]
If the input is not empty in this sense, but also does not begin with a valid lexeme after any leading whitespace and comments, lex returns [].

-}

readsTree2 :: (Read a) => ReadS (Tree' a)
readsTree2 s	= [(Branch l r, x)	|	("<", t)	<- lex s,
										(l, u)		<- readsTree2 t,
										("|", v)	<- lex u,
										(r, w)		<- readsTree2 v,
										(">", x)	<- lex w]
					++
					[(Leaf x, t)	|	(x, t)		<- reads s]

-- Make Tree an instance of Show
-- this allows us to use generic methods from Prelude to display and parse trees
--
instance (Show a) => Show (Tree' a) where
	showsPrec _ = showsTree1
instance (Read a) => Read (Tree' a) where
	readsPrec _ = readsTree2

-- Defining a lexical ordering for Tree'
instance (Ord a) => Ord (Tree' a) where
	(Leaf _)	<= (Branch _ _)		= True
	(Leaf x)	<= (Leaf y)			= x <= y
	(Branch _ _) 	<= (Leaf _)		= False
	(Branch l r)<= (Branch l' r')	= l <= l' || (l == l' && r <= r')