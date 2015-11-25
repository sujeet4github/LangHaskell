{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

{-
http://www.seas.upenn.edu/~cis194/spring13/hw/12-monads.pdf

In this assignment, you will write a simulator which could be used by Risk players
to estimate the probabilities of different outcomes before deciding on a course of
action.

-}

-- getting 3 random its using the MonadRandom package
-- http://hackage.haskell.org/package/MonadRandom
--
threeInts :: Rand StdGen (Int, Int, Int)
threeInts =
    do
        r1 <- getRandom
        r2 <- getRandom
        r3 <- getRandom
        return (r1,r2,r3)
threeIntsNoDo :: Rand StdGen (Int, Int, Int)
threeIntsNoDo =
        getRandom >>= \r1 ->
            getRandom >>= \r2 ->
                getRandom >>= \r3 ->
                    return (r1,r2,r3)

ex1 = evalRand threeInts (mkStdGen 10)
ex2 = evalRand threeIntsNoDo (mkStdGen 10)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army, battleLogs :: String }


instance Show Battlefield where
        show bf = "Battlefield (Attackers:\t"
                    ++ show (attackers bf)
                    ++ ", Defenders:\t"
                    ++ show (defenders bf)
                    ++ ")\n"
                    ++ (battleLogs bf)


------------------------------------------------------------
-- Exercise 1
--
-- DieValue represents the result of rolling a fair six-sided die.
--
testDieRoll = evalRandIO die

effectiveAttackers :: Battlefield -> Int
effectiveAttackers bf =
    let x = attackers bf
    in min 3 (max 0 (x-1))

effectiveDefenders :: Battlefield -> Int
effectiveDefenders bf =
    let x = defenders bf
    in min 2 (max 0 x)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf =
    do
        let a = attackers bf
        let d = defenders bf
        let attachingUnits = effectiveAttackers bf
        let defendingUnits = effectiveDefenders bf
        ats <- (replicateM attachingUnits die)
        bts <- (replicateM attachingUnits die)
        let sorted_ats = reverse $ sort ats
        let sorted_bts = reverse $ sort bts
        let b1 = "Attackers throw " ++ (show ats)
        let b2 = b1 ++ ", Defenders throw " ++ (show bts)
        let b3 = b2 ++ ", Attackers sorted throws " ++ (show sorted_ats)
        let b4 = b3 ++ ", Defenders sorted throws " ++ (show sorted_bts)
        let zipped = zip sorted_ats sorted_bts
        let comparisions = map (\(x,y) -> (compare x y)) zipped
        let removeEq = filter (/= EQ) comparisions
        if (length removeEq > 0)
            then if (head removeEq == GT)
                then do
                        let bl = b4 ++ ", Attackers win remain at " ++ (show a) ++ ", Defenders lose going to : " ++ show (d-1) ++ "\n"
                        let newBattleLog = (battleLogs bf) ++ bl
                        return $ Battlefield a (d-1) newBattleLog
                else do
                        let bl = b4 ++ ", Attackers lose going to " ++ (show (a-1)) ++ ", Defenders win remain at: " ++ show d ++ "\n"
                        let newBattleLog = (battleLogs bf) ++ bl
                        return $ Battlefield (a-1) d newBattleLog
            else do
                    let bl = b4 ++ ", Its a draw.\n"
                    let newBattleLog = (battleLogs bf) ++ bl
                    return $ Battlefield a d newBattleLog


invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
                let a = attackers bf
                let d = defenders bf
                if (a < 2) || (d < 1)
                    then return bf
                    else do
                            nbf <- battle bf
                            invade nbf


successfulInvasion :: Battlefield -> Bool
successfulInvasion bf = (defenders bf) == 0

successProb :: Battlefield -> Rand StdGen Double
successProb bf =
    let
        bfrs = replicateM 1000 (invade bf)
        successful = liftM (filter successfulInvasion) bfrs
        percentage = liftM (\xs -> 100 * fromIntegral (length xs) / 1000.0) successful
    in
        percentage

successProb2 :: Battlefield -> Rand StdGen Double
successProb2 bf =
    do
        bfrs <- replicateM 1000 (invade bf)
        let successful = filter successfulInvasion bfrs
        let successfulRatio = fromIntegral (length successful) / 1000.0
        let percentage = 100 * successfulRatio
        return percentage

-- from https://github.com/ibebrett/Haskell-CS194/blob/master/ex12/Risk.hs
successProbNth :: Battlefield -> Int -> Int -> Double -> Rand StdGen Double
successProbNth bf tryCount maxTries currProb =
    do
        nth <- invade bf
        let newProb = if (successfulInvasion nth) then (1+currProb) else currProb
        if (tryCount >= maxTries)
            then return newProb
            else successProbNth bf (1+tryCount) maxTries newProb
successProb3 :: Battlefield -> Rand StdGen Double
successProb3 bf = (successProbNth bf 1 1000 0)
                    >>= \v -> return ((v/1000.0)*100)

-- from https://github.com/evansb/cis194-hw/blob/master/hw12/Risk.hs
(|>) :: (b -> a) -> (a -> c) -> b -> c
(f |> g) x = g (f x)
successProb4 :: Battlefield -> Rand StdGen Double
successProb4 =  invade
                |> replicateM 1000
                |> liftM (filter successfulInvasion)
                |> liftM (\xs -> 100 * fromIntegral (length xs) / 1000.0)

succProbNTries bf try maxTries succCount =
    do
        nth <- invade bf
        let newSuccCount = if (successfulInvasion bf)
                        then succCount + 1
                        else succCount
        if (try >= maxTries)
            then return newSuccCount
            else succProbNTries bf (try+1) maxTries newSuccCount


example =
    let
        bf = Battlefield 10 10 ""
        x = replicateM 1000 (invade bf)
        lx = liftM (\x -> length x) x
        f = liftM (filter successfulInvasion) x
        lf = liftM (\x -> length x) f
        g = liftM (\xs -> fromIntegral (length xs) / 1000.0) x
    in
        undefined

main :: IO()
main = do
        b <- evalRandIO $ successProb $ Battlefield 4 10 ""
        print b
        b <- evalRandIO $ successProb2 $ Battlefield 4 10 ""
        print b
        b <- evalRandIO $ successProb3 $ Battlefield 4 10 ""
        print b
        b <- evalRandIO $ successProb4 $ Battlefield 4 10 ""
        print b