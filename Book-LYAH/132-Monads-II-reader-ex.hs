{-
http://dev.stephendiehl.com/hask/#reader-monad

The reader monad lets us access shared immutable state within a monadic context.

ask :: Reader r a
asks :: (r -> a) -> Reader r a
local :: (r -> b) -> Reader b a -> Reader r a
runReader :: Reader r a -> r -> a

-}
import Control.Monad.Reader

data MyContext = MyContext
                        { foo :: String
                        , bar :: Int }
                 deriving (Show)

computation :: Reader MyContext (Maybe String)
computation =
    do
        n <- asks bar
        x <- asks foo
        if n > 0
            then return (Just x)
            else return Nothing

ex1 :: Maybe String
ex1 = runReader computation $ MyContext "hello" 1

ex2 :: Maybe String
ex2 = runReader computation $ MyContext "haskell" 0