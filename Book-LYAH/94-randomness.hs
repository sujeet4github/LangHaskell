-- http://learnyouahaskell.com/input-and-output#randomness
--

import System.Random

-- random :: (RandomGen g, Random a) => g -> (a, g)
-- The RandomGen typeclass is for types that can act as sources of randomness.
-- The Random typeclass is for things that can take on random values

-- To use our random function, we have to get our hands on one of those random
-- generators. The System.Random module exports a cool type, namely StdGen that
-- is an instance of the RandomGen typeclass. We can either make a StdGen manually
-- or we can tell the system to give us one based on a multitude of sort of random stuff.
--
-- to manually make a StdGen - use mkStdGen :: Int -> StdGen
-- takes an integer and based on it gives us a random generator

genRandomNumber = random (mkStdGen 100) :: (Int, StdGen)

-- generate an infinite sequence
-- randoms :: (RandomGen g, Random a) => g -> [a]

-- randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
--  it takes as its first parameter a pair of values that set the lower and upper
--  bounds and the final value produced will be within those bounds.
--
-- randomRs :: (RandomGen g, Random a) => (a, a) -> g -> [a]
--  produces a stream of random values within our defined ranges

