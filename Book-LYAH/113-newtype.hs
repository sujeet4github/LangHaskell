-- newtype
-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids#the-newtype-keyword

{-
we've learned how to make our own algebraic data types by using the data keyword.
We've also learned how to give existing types synonyms with the type keyword.

There are 2 ways for a list to be an applicative,
 - One way is to have <*> take every function out of the list that is its left parameter
 and apply it to every value in the list that is on the right, resulting in every
 possible combination of applying a function from the left list to a value in the
 right list
 This is the default implementation.

 [(+1),(*100),(*5)] <*> [1,2,3]

 - The second way is to take the first function on the left side of <*> and apply it to
 the first value on the right, then take the second function from the list on the left
 side and apply it to the second value on the right, and so on. Ultimately, it's kind
 of like zipping the two lists together.
 But lists are already an instance of Applicative, so how did we also make lists an
 instance of Applicative in this second way? If you remember, we said that the ZipList
 a type was introduced for this reason, which has one value constructor, ZipList, that
 has just one field. We put the list that we're wrapping in that field. Then, ZipList
 was made an instance of Applicative, so that when we want to use lists as applicatives
 in the zipping manner, we just wrap them with the ZipList constructor.

 Control.Applicative.ZipList [(+1),(*100),(*5)] <*> Control.Applicative.ZipList [1,2,3]

 Now how would you write the data declaration for ZipList
 data ZipList a = ZipList { getZipList :: [a] }

 This looks fine and would actually work pretty well.

 The newtype keyword in Haskell is made exactly for these cases when we want to just
 take one type and wrap it in something to present it as another type.
 In the actual libraries, ZipList a is defined like this:

 newtype ZipList a = ZipList { getZipList :: [a] }

 Instead of the data keyword, the newtype keyword is used. Now why is that? Well for one,
 newtype is faster. If you use the data keyword to wrap a type, there's some overhead to
 all that wrapping and unwrapping when your program is running. But if you use newtype,
 Haskell knows that you're just using it to wrap an existing type into a new type (hence
 the name), because you want it to be the same internally but have a different type. With
 that in mind, Haskell can get rid of the wrapping and unwrapping once it resolves which
 value is of what type.

 When using newtype, you're restricted to just one constructor with one field.

-}

-- implement functor over 2-tuple so that function is mapped over 2nd parameter
-- no need to do any thing special, that is the default behavior
test2tuple = fmap (+4) (1,2)
-- implement functor over 2-tuple so that function is mapped over 1st parameter
-- so the type parameters are switched around, the 1st type parameter is the type
-- of the second component of the tuple.
newtype Pair b a = Pair { getPair :: (a, b) } deriving (Show)
-- should behave like fmap :: (a -> b) -> Pair c a -> Pair c b
-- ... remember c here represents the 2nd parameter which remains untouched
instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)
test1tuple = fmap (+4) (Pair (1,2))

{-

 The only thing that can be done with newtype is turning an existing type into a new type,
 so internally, Haskell can represent the values of types defined with newtype just like
 the original ones, only it has to keep in mind that the their types are now distinct.
 This fact means that not only is newtype faster, it's also lazier. Let's take a look at
 what this means.

 Haskell is lazy by default, which means that only when we try to actually print the
 results of our functions will any computation take place. Furthemore, only those
 computations that are necessary for our function to tell us the result will get carried
 out.

    e.g. head [3,4,5,undefined,2,undefined] is find
         tail on the same will throw an exception

-}

-- consider
-- a run-of-the-mill algebraic data type....defined using data
data CoolBool1 = CoolBool1 { getCoolBool1 :: Bool }
-- and a function
helloMe1 :: CoolBool1 -> String
helloMe1 (CoolBool1 _) = "hello"
-- the following throws an exception
-- helloMe1 undefined
-- Why?
-- Types defined with the data keyword can have multiple value constructors (even though
-- CoolBool only has one). So in order to see if the value given to our function conforms
-- to the (CoolBool _) pattern, Haskell has to evaluate the value just enough to see which
-- value constructor was used when we made the value. And when we try to evaluate an
-- undefined value, even a little, an exception is thrown.
--
-- lets try using newtype instead of data
-- remember restriction - only one data constructor with one field
newtype CoolBool2 = CoolBool2 { getCoolBool2 :: Bool }
helloMe2 :: CoolBool2 -> String
helloMe2 (CoolBool2 _) = "hello"
-- Now
-- helloMe2 undefined
-- does not throw an exception, because, haskell does not evaluate the parameter
-- because it does not need to...
-- when we use newtype, Haskell can internally represent the values of the new type in the
-- same way as the original values. It doesn't have to add another box around them, it just
-- has to be aware of the values being of different types. And because Haskell knows that
-- types made with the newtype keyword can only have one constructor, it doesn't have to
-- evaluate the value passed to the function to make sure that it conforms to the
-- (CoolBool _) pattern because newtype types can only have one possible value constructor
-- and one field!

-- This difference in behavior may seem trivial, but it's actually pretty important
-- because it helps us realize that even though types defined with data and newtype
-- behave similarly from the programmer's point of view because they both have value
-- constructors and fields, they are actually two different mechanisms. Whereas data
-- can be used to make your own types from scratch, newtype is for making a completely
-- new type out of an existing type. Pattern matching on newtype values isn't like
-- taking something out of a box (like it is with data), it's more about making a direct
-- conversion from one type to another.


-- type vs. newtype vs. data
-- -------------------------
--
-- The type keyword is for making type synonyms. What that means is that we just give
-- another name to an already existing type so that the type is easier to refer to.
-- Say we did the following:
type IntList = [Int]
-- All this does is to allow us to refer to the [Int] type as IntList. They can be
-- used interchangeably. We don't get an IntList value constructor or anything like
-- that.
-- Because [Int] and IntList are only two ways to refer to the same type, it doesn't
-- matter which name we use in our type annotations:
test_type = ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])

-- The newtype keyword is for taking existing types and wrapping them in new types,
-- mostly so that it's easier to make them instances of certain type classes.
-- When we use newtype to wrap an existing type, the type that we get is separate from the original type. If we make the following newtype:
newtype CharList = CharList { getCharList :: [Char] }
-- we can't use ++ to put together a CharList and a list of type [Char].
-- We can't even use ++ to put together two CharLists, because ++ works only on lists
-- and the CharList type isn't a list, even though it could be said that it contains one.
-- We can, however, convert two CharLists to lists, ++ them and then convert that back
-- to a CharList.
--
-- When we use record syntax in our newtype declarations, we get functions for converting
-- between the new type and the original type: namely the value constructor of our newtype
-- and the function for extracting the value in its field.
--
-- The new type also isn't automatically made an instance of the type classes that the
-- original type belongs to, so we have to derive or manually write them.
--
-- In practice, you can think of newtype declarations as data declarations that can only
-- have one constructor and one field.
-- If you catch yourself writing such a data declaration, consider using newtype.

-- The data keyword is for making your own data types and with them, you can go hog wild.
-- They can have as many constructors and fields as you wish and can be used to implement
-- any algebraic data type by yourself.

-- If you just want your type signatures to look cleaner and be more descriptive, you
-- probably want type synonyms. If you want to take an existing type and wrap it in a
-- new type in order to make it an instance of a type class, chances are you're looking
-- for a newtype. And if you want to make something completely new, odds are good that
-- you're looking for the data keyword.
