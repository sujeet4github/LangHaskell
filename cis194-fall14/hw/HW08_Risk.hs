module HW08_Risk where

{-
http://www.seas.upenn.edu/~cis194/fall14/hw/08-monads.pdf
second part:

Risk
====
The game of Risk involves two or more players, each vying to “conquer
the world” by moving armies around a board representing the
world and using them to conquer territories. The central mechanic of
the game is that of one army attacking another, with dice rolls used
to determine the outcome of each battle.

The rules of the game make it complicated to determine the likelihood
of possible outcomes. In this assignment, you will write a
simulator which could be used by Risk players to estimate the probabilities
of different outcomes before deciding on a course of action.

The Rules of Battle:
===================
The rules of attacking in Risk are as follows.
• There is an attacking army (containing some number of units) and
    a defending army (containing some number of units).
• The attacking player may attack with up to three units at a time.

However, they must always leave at least one unit behind. That
is, if they only have three total units in their army they may only
attack with two, and so on.
• The defending player may defend with up to two units (or only
one if that is all they have).
• To determine the outcome of a single battle:
    the attacking and defending players each roll one six-sided die
    for every unit they have attacking or defending.
    So the attacking player rolls one, two, or three dice, and the
    defending player rolls one or two dice.
• The attacking player sorts their dice rolls in descending order.
    The defending player does the same.
• The dice are then matched up in pairs, starting with the highest roll
    of each player, then the second-highest.
• For each pair, if the attacking player’s roll is higher, then one of
    the defending player’s units die. If there is a tie, or the defending
    player’s roll is higher, then one of the attacking player’s units die.

For example, suppose player A has 3 units and player B has 5.
A can attack with only 2 units, and B can defend with 2 units.
So A rolls 2 dice, and B does the same.
Suppose A rolls a 3 and a 5, and B rolls a 4 and a 3.
After sorting and pairing up the rolls, we have
        A B
        5 4
        3 3

A wins the first matchup (5 vs. 4), so one of B’s units dies.
The second matchup is won by B, however (since B wins ties),
so one of A’s units dies.
The end result is that now A has 2 units and B has 4.

If A wanted to attack again they would only be able to attack with 1
unit (whereas B would still get to defend with 2—clearly this would
give B an advantage because the higher of B’s two dice rolls will get
matched with A’s single roll.)

-}

{-
The StdRand monad
=================
Since battles in Risk are determined by rolling dice, your simulator
will need some way to access a source of non-determinism,
called a random generator

You will have to get many pseudo-random numbers
out of your random generator, and keep track of the way that the random
generator changes as you query it for more randomness. This
sounds like the perfect time to use a monad! The monad will keep
the random generator for you and allow you to sequence queries.
The MonadRandom package provides just such a monad.
If you don’t already have it installed, just say cabal install MonadRandom
at your command line.

import Control.Monad.Random.
You can find the type Rand in that package, which defines a randomness
monad for any type of random generator g. For this homework,
we won’t need to fiddle with the type of the generator — we’ll just
use the StdGen standard random generator

-}
import Control.Monad.Random
import Data.List (sortBy)

type Army = Int

data ArmyCounts = ArmyCounts {
        attackers :: Army,
        defenders :: Army
    } deriving (Show, Eq)


type DieRoll = Int

-- put the following line to get rid of extra random generator parameter
type StdRand = Rand StdGen

{-
Examples using Control.Monad.Random
from http://hackage.haskell.org/package/MonadRandom-0.3/docs/Control-Monad-Random.html
-}
die :: (RandomGen g) => Rand g Int
die = getRandomR (1,6)

diceRolls :: (RandomGen g) => Int -> Rand g [Int]
diceRolls n = sequence (replicate n die)

show20DiceRolls = do
  values <- evalRandIO (diceRolls 20)
  putStrLn (show values)

-- Ex 3
dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1,6)
-- to test in repl:  evalRandIO dieRoll

-- Hint: This function (and the next) can be much simplified if you
-- write and use a Monoid instance for ArmyCounts
instance Monoid ArmyCounts
    where
        mappend a1 a2 = ArmyCounts na nd
            where
                na = (attackers a1)+(attackers a2)
                nd = (defenders a1)+(defenders a2)
        mempty = ArmyCounts 0 0

-- Ex 4
-- takes the attacker’s dice rolls and the defender’s dice rolls and
-- computes the change in the number of armies resulting from the rolls.
battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults attackerDiceRolls defenderDiceRolls = mconcat resultingArmyCounts
    where
        descendingOrder x y = compare y x
        sortDice = sortBy descendingOrder
        zippedSortedDiceRollPairs = zip (sortDice attackerDiceRolls) (sortDice defenderDiceRolls)
        resultingArmyCounts = map battleResult zippedSortedDiceRollPairs

battleResult :: (DieRoll, DieRoll) -> ArmyCounts
battleResult (adr, ddr)
    | adr > ddr = ArmyCounts 0 (-1)
    | otherwise = ArmyCounts (-1) 0

-- Ex 5
-- simulates a single battle (as explained above) between two
-- opposing armies
battle :: ArmyCounts -> StdRand ArmyCounts
battle ac = do
    let attackArmies = min ((attackers ac) - 1) 3
    attackerDiceRolls <- diceRolls attackArmies
    let defenceArmies = min (defenders ac) 2
    defenderDiceRolls <- diceRolls defenceArmies
    let battleResultsVal = battleResults attackerDiceRolls defenderDiceRolls
    return $ mappend ac battleResultsVal

-- Ex 6
-- simulates an entire invasion attempt: that is, repeated calls
-- to battle until there are no defenders remaining, or fewer than two
-- attackers.
invade :: ArmyCounts -> StdRand ArmyCounts
invade ac
    | (attackers ac) < 2    = return ac
    | (defenders ac) < 1    = return ac
    | otherwise = battle ac >>= invade

-- Ex 7
-- runs invade 1000 times, and uses the results to compute a
-- Double between 0 and 1 representing the estimated probability that
-- the attacking army will completely destroy the defending army
--
-- You will likely need this function, provided as we haven’t talked
-- much about numeric conversions:
(//) :: Int -> Int -> Double
a // b = fromIntegral a / fromIntegral b
--
-- e.g if the defending army is destroyed in 300 of the 1000
-- simulations (but the attacking army is reduced to 1 unit in the other
-- 700), successProb should return 0.3.
successfulInvasion :: ArmyCounts -> Bool
successfulInvasion (ArmyCounts a d) = d <= 0 && a > 1
--
successProb :: ArmyCounts -> StdRand Double
successProb ac = do
    let sampleSize = 1000
    results <- sequence $ replicate sampleSize $ invade ac
    let positives = length $ filter successfulInvasion results
    return $ (positives // sampleSize)

