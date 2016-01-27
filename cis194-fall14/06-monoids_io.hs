{-
http://www.seas.upenn.edu/~cis194/fall14/lectures/06-monoid-io.html

CIS 194 Week 6 2 October 2014
-}
import Prelude hiding ( Functor(..) )   -- we define this ourselves, below
{-
Further reading

Real World Haskell, Chapter 7 Learn You a Haskell, Chapter 9

IO
==

GHC looks for a module named Main to find the main action. If you omit a module header
on a Haskell file, the module name defaults to Main, so this often works out, even if
the filename is not Main.hs. If you wish to use a module name other than Main, you have
to use a command-line option when calling ghc or runhaskell. Say you have a file
Something.hs that looks like:

module Something where
main :: IO ()
main = putStrLn "Hi out there!"

You can compile that with ghc --make -main-is Something Something.hs.
Note the double dashes with --make but only a single dash with -main-is.

-}

sillyExchange :: IO ()
-- () type is pronounced “unit” and has one value, ()
-- It’s as if it was declared with : data () = () -(Not valid syntax)
--
sillyExchange = do
  putStrLn "Hello, user!"
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Pleased to meet you, " ++ name ++ "!"


{-
Notes for next function: jabber

1.
readFile :: FilePath -> IO String, where type FilePath = String.
This function reads in the entire contents of a file into a String.

2.
let statements within do blocks.
It would be awfully silly if all of the pure programming we have covered
were unusable from within do blocks.
The let statement in a do block allows you to create a new variable bound
to a pure value. Note the lack of in.
Remember that when you say let x = y, a and b have the same types.
When you say x <- y, y has to have a type like IO a, and then x has type a.

3.
return :: a -> IO a. If you need to turn a pure value into an I/O action,
use return. return is a regular old function in Haskell. It is not the same
as return in C/C++ or Java
let x = y is the same as x <- return y, but the former is vastly preferred:
it makes the purity of y more obvious.

-}

jabber :: IO ()
jabber = do
  wocky <- readFile "jabberwocky.txt"
  let wockylines = drop 2 (lines wocky)  -- discard title
  count <- printFirstLines wockylines
  putStrLn $ "There are " ++ show count ++ " stanzas in Jabberwocky."

printFirstLines :: [String] -> IO Int
printFirstLines ls = do
  let first_lines = extractFirstLines ls
  putStr (unlines first_lines)
  return $ length first_lines

extractFirstLines :: [String] -> [String]
extractFirstLines []         = []
extractFirstLines [_]        = []
extractFirstLines ("" : first : rest)
  = first : extractFirstLines rest
extractFirstLines (_ : rest) = extractFirstLines rest


{-
Monoids
=======

Consider some type m and an operation (<>) :: m -> m -> m.
The type and operation form a monoid when there exists a particular element
    mempty :: m such that x <> mempty == x and mempty <> x == x;
    and the operation (<>) is associative.
        That is, (a <> b) <> c == a <> (b <> c).

Monoids are actually a mathematical concept, but they are ubiquitous in programming.
This is true in all languages, but we make their presence in Haskell much more explicit,
through the use of a type class:

There are a great many Monoid instances available. Perhaps the easiest one is for lists:
instance Monoid [a] where
  mempty  = []
  mappend = (++)

Functor
=======
You can think of functors as being containers, where it is possible to twiddle the contained bits.
The fmap operation allows you access to the contained bits, without affecting the container.

When dealing with containers that you know nothing about, a Functor instance is often all
you need to make progress.

Record Syntax
=============
data D = C T1 T2 T3
We could also declare this data type with record syntax as follows:
data D = C { field1 :: T1, field2 :: T2, field3 :: T3 }
1. Each field name is automatically a projection function
2. There is special syntax for constructing, modifying (constructing a new value same as original
   but with changes specified), and pattern-matching

-}
