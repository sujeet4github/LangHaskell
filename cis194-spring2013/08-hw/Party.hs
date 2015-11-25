{-# OPTIONS_GHC -fno-warn-orphans #-}
-- http://www.seas.upenn.edu/~cis194/spring13/hw/08-IO.pdf
module Party where

import Employee
import Test.QuickCheck
import Data.Monoid
import Data.Tree
import Text.Printf
import Data.List

-- Exercise 1
-- ----------
-- adds an Employee to the GuestList (updating the cached
-- Fun score appropriately)
--  Of course, in general this is impossible:
--   the updated fun score should depend on whether the Employee
--   being added is already in the list, or if any of their direct
--   subordinates are in the list, and so on. For our purposes,
--   though, you may assume that none of these special cases will
--   hold:
--   that is, glCons should simply add the new Employee and
--   add their fun score without doing any kind of checks.
-- 1.
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es groupFun) = GL (es++[e]) (groupFun + empFun e)

e_GL :: GuestList -> [Employee]
e_GL (GL es _) = es
f_GL :: GuestList -> Integer
f_GL (GL _ f) = f
prop_glCons :: Employee -> GuestList -> Bool
prop_glCons e gl = empAdded && funAdded
                    where
                        newGl = glCons e gl
                        employeesInNewGl = e_GL newGl
                        empAdded = case employeesInNewGl of
                          (e:_) -> True
                          otherwise -> False
                        funAdded = if (f_GL newGl == (f_GL gl) + (empFun e))
                                    then True
                                    else False

-- 2. A Monoid Instance for GuestList
instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL ex fx) (GL ey fy) = GL (ex++ey) (fx+fy)

prop_gl_verifyMonoidId :: GuestList -> Bool
prop_gl_verifyMonoidId gl = gl <> mempty == gl && gl == mempty <> gl
prop_gl_verifyMonoidAssoc :: GuestList -> GuestList -> GuestList -> Bool
prop_gl_verifyMonoidAssoc x y z = x <> (y <> z) == (x <> y) <> z

-- 3.
-- takes two GuestLists and returns whichever one of them
-- is more fun, i.e. has the higher fun score.
-- (If the scores are equal it does not matter which is returned.)
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

prop_moreFun :: Integer -> Integer -> Property
prop_moreFun x y = x > y ==> (moreFun x_gl y_gl) == x_gl
                where
                  x_gl = GL [] x
                  y_gl = GL [] y

instance Arbitrary Employee where
  arbitrary = genEmployees
genEmployees :: Gen Employee
genEmployees = do
                n <- arbitrary
                f <- arbitrary
                return (Emp n f)

-- need sized to prevent really uncontrollably large groups
instance Arbitrary GuestList where
  arbitrary = sized arbitraryGuestList
arbitraryGuestList :: Int -> Gen GuestList
arbitraryGuestList m = do
                          n <- choose (0, m)
                          ts <- vectorOf n (genEmployees)
                          return (foldr glCons mempty ts)

-- Exercise 2
-- ----------
-- To implement a fold for rose tree defined in Data.Tree
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) (map (treeFold f) (subForest t))

-- Exercise 4
-- ----------
-- to figure out who to invite to the party in order to
-- maximize the total amount of fun
-- depends of Exercises 1, 2 & 3
maxFun :: Tree Employee -> GuestList
-- uncurry converts a curried function to a function on pairs.
-- uncurry :: (a -> b -> c) -> ((a, b) -> c)
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 3
-- ----------
-- The first argument is the “boss” of the current subtree
--  (let’s call him Bob).
-- The second argument is a list of the results
--   for each subtree under Bob.
--
-- Each result is a pair of GuestLists:
--  the first GuestList in the pair is the best possible guest list
--  with the boss of that subtree;
--  the second is the best possible guest list without the
--  boss of that subtree.
-- nextLevel should then compute the overall best guest list that includes
-- Bob, and the overall best guest list that doesn’t include Bob.
--
-- if including the boss at this level, dont include the bosses of sublevels
-- since then they cant have fun :-)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = (withBoss, withoutBoss)
            where
              -- uncurry converts a curried function to a function on pairs.
              -- uncurry :: (a -> b -> c) -> ((a, b) -> c)
              bestPossibleSubList = mconcat (map (uncurry moreFun) results)
              withoutBoss = bestPossibleSubList
              withBoss = glCons boss (mconcat (map snd results))

main :: IO ()
main = main1


parse :: GuestList -> String
parse (GL empl0 fun) = "Fun score : "
                    ++ show fun ++ "\n"
                    ++ unlines (sort (map empName empl0))
main1 :: IO ()
main1 = readFile "company.txt"
        >>= putStrLn . parse . maxFun . read

getNames :: GuestList -> String
getNames (GL emps fun) = foldl (\x y -> x++"\n"++y) "" (sort (map empName emps))

main2 :: IO ()
main2 = readFile "company.txt"
        >>= (\l -> showFunDisplay (maxFun (read l)))
main2' :: IO ()
main2' = readFile "company.txt"
        >>= showFunDisplay . maxFun . read
showFunDisplay :: GuestList -> IO()
showFunDisplay mf = printf "Total fun: %d" (f_GL mf) >> putStrLn (getNames mf)
