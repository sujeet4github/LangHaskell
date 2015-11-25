module Vector where

-- it's a very strong convention in Haskell to never add typeclass constraints in data declarations.
-- only in individual method declarations
--

data Vector a = Vector a a a deriving (Show)
-- When declaring a data type, the part before the = is the type constructor
-- and the constructors after it (possibly separated by |'s) are value constructors.
--
-- A value constructor can take some values parameters and then produce a new value.
-- type constructors can take types as parameters to produce new types.



--  if you examine the type declaration for these functions, you'll see that they
-- can operate only on vectors of the same type and the numbers involved must also
-- be of the type that is contained in the vectors.
-- Notice that we didn't put a Num class constraint in the data declaration,
-- because we'd have to repeat it in the functions anyway, AND, because of convention listed above
--
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

