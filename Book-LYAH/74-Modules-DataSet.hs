-- http://learnyouahaskell.com/modules#data-set
--

{-

names in Data.Set clash with a lot of Prelude and Data.List names

All the elements in a set are unique. And because they're internally implemented
with trees (much like maps in Data.Map), they're ordered.

Checking for membership, inserting, deleting, etc. is much faster
than doing the same thing with lists.

The most common operation when dealing with sets are inserting into a set,
checking for membership and converting a set to a list.


fromList takes a list and converts it into a set.
toList

union
intersection
difference
null, size, member, empty, singleton, insert and delete

isSubsetOf
isProperSubsetOf - Set A is a proper subset of set B if B contains all the elements that A does but has more elements.

map, filter

-}

import qualified Data.Set as Set
