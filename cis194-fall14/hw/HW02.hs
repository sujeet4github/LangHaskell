{-
http://www.seas.upenn.edu/~cis194/fall14/extras/02-dict/HW02.hs

Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List

{-
You are allowed (and encouraged) to use functions in the Data.List
standard library. You can find a list of these functions at:
http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html

The type signatures tell you much about what these functions do.
To experiment with them, just say import Data.List in GHCi and
start trying out expressions. Remember that a String is just a list of
characters (that is, a [Char]), and so provides conveniently-written
test data.

You will be writing functions to help in a computer player for a
Scrabble game. Though familiarity with the rules is not assumed for
this assignment, you may wish to read them at
http://www.hasbro.com/scrabble/en_US/discover/rules.cfm.



-}

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
{-
Exercise 1
The first function you will write tests whether a certain
word is formable from the tiles in a Scrabble hand. That is, given a
String and a list of Chars, can the String be formed from the Chars,
taking any duplicates into account?

Hint: Start by thinking what this should do if the string to be
matched is empty. Then, what should it do if the string is nonempty?
The elem and delete functions from Data.List may be helpful here.
-}
formableBy :: String -> Hand -> Bool
formableBy "" _         = True
formableBy (c:cs) hand  = if c `elem` hand
                            then formableBy cs (delete c hand)
                            else False

t1 = formableBy "fun" ['x','n','i','f','u','e','l'] == True
t2 = formableBy "haskell" ['k','l','e','h','a','l','s'] == True
t3 = formableBy "haskell" ['k','l','e','h','a','y','s'] == False

{-
Exercise 2 Now, using formableBy, write a function wordsFrom that
gives a list of all valid Scrabble words formable from a certain hand.
The Words module (imported by the HW02.hs you downloaded) allows
you to use allWords :: [String], which contains all valid
Scrabble words.
-}

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

{-
Exercise 3 Most plays in Scrabble do not build completely fresh
words from the tiles in one's hand. All Scrabble plays (except the
first) have to build on existing tiles. Often, there is a place that a
player wants to make a word, but that player must figure out if a
word can fit. Your next functions will help to solve this part of the
problem.

A template is a string containing some letters and some question marks.
The question marks represent open spaces on the board that will get
filled in by the letter in a player's hand. The letters in the template
represent letters that already appear on the board. They must appear
in exactly the same positions in the final words produced. So, the
template ??r? represents a board position where the third letter of a
four-letter word must be r.
-}

-- checks to see if a given word matches a template, given
-- a set of tiles available
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate template hand word =   lengthMatches &&
                                        nonQuestionsMatch &&
                                        formableBy subWordToMatch hand
    where
        lengthMatches = length(template) == length(word)
        zipped = zip template word
        individualLettersMatch = map (\(x,y) -> x == '?' || x == y) zipped
        nonQuestionsMatch = 0 == length ( filter (== False) individualLettersMatch )
        (pattern, subWordToMatch) = foldr gatherUpSubWord ("","") zipped

gatherUpSubWord ('?',c) (qnMarks, subword) = ( ('?':qnMarks), (c:subword) )
gatherUpSubWord _ x = x

t4 = wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care" == True
t5 = wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care" == False
t6 = wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car" == False
t7 = wordFitsTemplate "let" ['x','x'] "let" == True


{-
Exercise 4 Now, using that function, write another one that produces
all valid Scrabble words that match a given template using a
hand of available tiles. This will be similar to wordsFrom
-}
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = filter (wordFitsTemplate template hand) allWords

{-
Exercise 5 Now we must think about scoring, as not all words in
Scrabble are created equal! The Words module, along with providing
cis 194: homework 2 6
allWords, provides scrabbleValue :: Char -> Int that gives the
point value of any letter. Use that function to write a new function
that gives the point value of any word.
-}
scrabbleValueWord :: String -> Int
scrabbleValueWord x = foldr (+) 0 $ map scrabbleValue x

t8 = scrabbleValueWord "care" == 6
t9 = scrabbleValueWord "quiz" == 22


{-
Exercise 6 You will use the scrabbleValueWord function to write a
filtering function that takes a list of words and selects out only those
that have the maximum point value. Note that there may be many
words tied for the most points; your function must return all of them.

-}
bestWords :: [String] -> [String]
bestWords ws = let
                wordsAndPoints = zip ws (map scrabbleValueWord ws)
                fold_function (w,p) (ws,ap)
                    | p > ap    = ([w],p)
                    | p == ap   = ((w:ws),p)
                    | otherwise = (ws,ap)
                bestOfTuple = foldr fold_function ([],0) wordsAndPoints
               in
                fst bestOfTuple

t10 = bestWords (wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l']) == ["carb"]
t11 = sort (bestWords ["cat", "rat", "bat"]) == ["bat","cat"]
t12 = bestWords [] == []

{-
Exercise 7 A Scrabble board is not a completely blank canvas. There
are four kinds of special squares: double-letter, triple-letter, doubleword,
and triple-word. A letter played on a double- or triple-letter
square gets its point value multiplied, and if any letter is played
on a double- or triple-word square, the whole word’s value gets
multiplied. The effects multiply with each other, as appropriate. So,
a play on both a double-word and a triple-word gets multiplied by 6.
If one tile is on a double-letter and another is on a double-word, then
that letter’s value is multiplied by 4.
To represent these special squares, we use a new type STemplate
(the S is for "square"). A stemplate is like a template, but it uses ’D’
and ’T’ to mark double- and triple-letter squares, respectively, and
it uses ’2’ and ’3’ to mark double- and triple-word squares, respectively.
Thus, the stemplate ?e??3 represents a place on the board
where there is room for a 5-letter word, the second letter of which is
an e, and the last letter of which falls on a triple-word square.
-}

-- Write a function scrabbleValueTemplate that computes the value
-- of playing a given word on a given template. In this function, you
-- may assume that the word actually matches the template
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate t s =
    let
        hasDW   = '2' `elem` t
        hasTW   = '3' `elem` t
        mult1   = if (hasDW) then 2 else 1
        mult    = if (hasTW) then mult1*3 else mult1
        calc ('T',x) = 3 * mult * scrabbleValue x
        calc ('D',x) = 2 * mult * scrabbleValue x
        calc ('?',x) = 1 * mult * scrabbleValue x
        calc ( _ ,x) = 1 * mult * scrabbleValue x
    in
        sum $ map calc $ zip t s


t13 = scrabbleValueTemplate "?e??3" "peace" == 27
t14 = scrabbleValueTemplate "De?2?" "peace" == 24
t15 = scrabbleValueTemplate "??Tce" "peace" == 11
