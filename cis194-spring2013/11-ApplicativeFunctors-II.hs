
import Prelude hiding (Functor, fmap, Applicative, pure, (<*>))

-- Review of Part-I
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- Every Applicative is also a Functor
--  so can we implement fmap in terms of pure and (<*>)? Let’s try!
-- fmap g x = pure g <*> x
-- we stipulate as a law that this equality must hold—this is a formal
-- way of stating that the Functor and Applicative instances for a
-- given type must “play nicely together”.

instance Functor Maybe where
  fmap g x = pure g <*> x
instance Applicative Maybe where
  pure              = Just
  Nothing <*> _     = Nothing
  _ <*> Nothing     = Nothing
  Just f <*> Just x = Just (f x)

liftA2 :: (Functor f, Applicative f) => (a -> b -> c) -> (f a -> f b -> f c)
liftA2 h fa fb = (h `fmap` fa) <*> fb

useCase_ApplicatorMaybe m1 m2 m3 =
            (m1 .+ m2) .* m3
                    where
                      (.+) = liftA2 (+)
                      (.*) = liftA2 (*)
test_uc2_1 = useCase_ApplicatorMaybe (Just 4) (Just 5) (Just 8)
test_uc2_2 = useCase_ApplicatorMaybe (Just 3) Nothing (Just 8)

-- Applicative for Lists
-- Two possible ways to do it:
-- default way is to do all possible combinations
useCase_AllPossibleCombinations =
            ([4,5] .* pure 2) .+ [6,1]
                    where
                      (.+) = liftA2 (+)
                      (.*) = liftA2 (*)
inappropriateUse_AllPossibleCombinations =
  (Employee <$> names) <*> phones
    where
      names  = ["Joe", "Sara", "Mae"]
      phones = ["555-5555", "123-456-7890", "555-4321"]

instance Functor [] where
  fmap g x = pure g <*> x

instance Applicative [] where
  pure a = [a]   -- a "deterministic" value
  [] <*> _ = []
  (f:fs) <*> as = (map f as) ++ (fs <*> as)

-- other way is to match up elementwise - like zip
-- We implement the instance using a newtype wrapper to distinguish it
-- from the other list instance.
newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Show)
instance Functor ZipList where
  fmap g x = pure g <*> x
instance Applicative ZipList where
  -- we must obey the law
  --   pure f <*> xs === f <$> xs
  -- since RHS is a list with same length as xs, formed by applying
  --   f to every element in xs
  -- only way to get LHS == RHS is
  -- to make pure create infinite copies of f, since we dont know how
  -- long xs is going to be
  pure = ZipList . repeat
  ZipList fs <*> ZipList as = ZipList (zipWith ($) fs as)
test_MatchupArguments =
  ZipList [(2*),(5*),(9*)] <*> ZipList [1,4,7]
test_MatchupArguments_Employees =
  (Employee `fmap` n) <*> p
    where
       names  = ["Joe", "Sara", "Mae"]
       phones = ["555-5555", "123-456-7890", "555-4321"]
       n = ZipList names
       p = ZipList phones

type Name = String
type PhoneNumber = String
data Employee = Employee { name :: Name,
                           phone :: PhoneNumber}
                           deriving (Show)

-- Reader from an Environment
--
instance Functor ((->) e) where
  fmap = (.)
instance Applicative ((->) e) where
  pure = const
  f <*> x = \e -> (f e) (x e)
-- This is known as the reader or environment applicative, since it allows
-- “reading” from the “environment” e.
--
data BigRecord = BR { getName         :: Name
                    , getSSN          :: String
                    , getSalary       :: Integer
                    , getPhone        :: String
                    , getLicensePlate :: String
                    , getNumSickDays  :: Int
                    }

r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2

getEmp :: BigRecord -> Employee
getEmp = (Employee `fmap` getName) <*> getPhone

ex01 = getEmp r

-- The Applicative API
--
-- One of the benefits of having a unified interface like Applicative is
-- that we can write generic tools and control structures that work with
-- any type which is an instance of Applicative.

-- takes two values and pairs them, but all in the context of some Applicative f.
pair :: Applicative f => f a -> f b -> f (a,b)
-- take a function to "lift" over arguments using fmap and apply
pair_v1 fa fb = ( (\x y -> (x,y)) `fmap` fa) <*> fb
-- special syntax for pair constructor (,)
pair_v2 fa fb = ( (,) `fmap` fa) <*> fb
-- this is the liftA2 pattern ...
pair_v3 fa fb = liftA2 (,) fa fb
-- point free style
pair = liftA2 (,)
