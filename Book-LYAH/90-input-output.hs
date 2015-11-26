-- http://learnyouahaskell.com/input-and-output
--
{-

An I/O action is something that, when performed, will carry out an action with a
side-effect (that's usually either reading from the input or printing stuff to
the screen) and will also contain some kind of return value inside it.

The empty tuple is a value of () and it also has a type of ().

I/O actions will only be performed when they are given a name of main or when
they're inside a bigger I/O action that we composed with a do block.
We can also use a do block to glue together a few I/O actions and then we can
use that I/O action in another do block and so on.
Either way, they'll be performed only if they eventually fall into main.
NOTE: there's also one more case when I/O actions will be performed.
When we type out an I/O action in GHCI and press return, it will be performed.

Note: IO is a box like [], Maybe etc, it has kind * -> *

return is sort of the opposite to <-.
While return takes a value and wraps it up in a box, <- takes a box (and performs it)
and takes the value out of it, binding it to a name.

Methods:
-------
putStrLn :: String -> IO ()
putStr :: String -> IO ()
putChar :: Char -> IO()

-- it's just putStrLn . show
print :: Show a => a -> IO ()

getLine :: IO String
getChar :: IO Char

import Control.Monad to use:

Control.Monad.when :: Applicative f => Bool -> f () -> f ()
    useful for encapsulating the if something then do some I/O action else return () pattern.
Control.Monad.sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)
    takes a list of I/O actions and returns an I/O actions that will perform those actions one after the other.
    if container m is a list, it returns a list...if it was a Either it will return an Either etc etc

-- Because mapping a function that returns an I/O action over a list and then sequencing
-- it is so common, the utility functions mapM and mapM_ were introduced.
-- mapM takes a function and a list, maps the function over the list and then sequences it.
-- mapM_ does the same, only it throws away the result later.
-- We usually use mapM_ when we don't care what result our sequenced I/O actions have.
mapM :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
mapM_ :: (Monad m, Foldable t) => (a -> m b) -> t a -> m ()

Control.Monad.forever :: Monad m => m a -> m b
    takes an I/O action and returns an I/O action that just repeats the I/O action it got forever.

Control.Monad.forM :: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
    is like mapM, only that it has its parameters switched around.

To reiterate, I/O actions are values much like any other value in Haskell. We can pass them as parameters
to functions and functions can return I/O actions as results. What's special about them is that if they
fall into the main function (or are the result in a GHCI line), they are performed. And that's when they
get to write stuff on your screen or play Yakety Sax through your speakers. Each I/O action can also
encapsulate a result with which it tells you what it got from the real world.

-}


