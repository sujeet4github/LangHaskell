-- http://learnyouahaskell.com/modules#data-map
--

{-

Association lists (also called dictionaries) are lists that are used to store key-value
pairs where ordering doesn't matter.

Data.Map exports functions that clash with the Prelude and Data.List ones.

fromList function takes an association list (in the form of a list) and returns a map
    with the same associations.

toList is the inverse of fromList

empty represents an empty map

insert takes a key, a value and a map and returns a new map that's just like the old one,
    only with the key and value inserted.

null checks if a map is empty.

size reports the size of a map.

singleton takes a key and a value and creates a map that has exactly one mapping.

lookup works like the Data.List lookup, only it operates on maps. It returns Just something if it finds something for the key and Nothing if it doesn't.

member is a predicate takes a key and a map and reports whether the key is in the map or not.

map and filter work much like their list equivalents.

keys and elems return lists of keys and values respectively.

fromListWith is a cool little function. It acts like fromList, only it doesn't discard
    duplicate keys but it uses a function supplied to it to decide what to do with them.

-}

import qualified Data.Map as Map

-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#type-synonyms
type Name = String
type PhoneNumber = String
-- Type synonyms can also be parameterized
type AssocList k v = [(k,v)]

phoneBook :: AssocList Name PhoneNumber
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

type PhoneBook = Map.Map Name PhoneNumber

phoneBookToMap :: AssocList Name PhoneNumber -> PhoneBook

type PhoneBook2 = Map.Map Name [PhoneNumber]
phoneBookToMapUsingList :: AssocList Name PhoneNumber -> PhoneBook2

-- If a duplicate key is found, the function we pass is used to combine the values of
-- those keys into some other value.
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
phoneBookToMapUsingList xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

test_phoneBook = Map.lookup "patsy" $ phoneBookToMap phoneBook
test_phoneBook2 = Map.lookup "patsy" $ phoneBookToMapUsingList phoneBook

{-

insertWith is to insert what fromListWith is to fromList.
    It inserts a key-value pair into a map, but if that map already contains the key,
    it uses the function passed to it to determine what to do.


-}
