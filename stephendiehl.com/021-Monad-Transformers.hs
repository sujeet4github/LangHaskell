{-
See 02-Monads.hs

Monad Transformers
==================
mtl / transformers

So the descriptions of Monads in the previous chapter are a bit of a white lie.
Modern Haskell monad libraries typically use a more general form of these
written in terms of monad transformers which allow us to compose monads
together to form composite monads. The monads mentioned previously are subsumed
by the special case of the transformer form composed with the Identity monad.

Monad   |   Transformer |   Type    |   Transformed Type
=====       ===========     ====        ================
Maybe       MaybeT          Maybe a     m (Maybe a)
Reader      ReaderT         r -> a      r -> m a
Writer      WriterT         (a,w)       m (a,w)
State       StateT          s -> (a,s)  s -> m (a,s)

type State  s = StateT  s Identity
type Writer w = WriterT w Identity
type Reader r = ReaderT r Identity

instance Monad m => MonadState s (StateT s m)
instance Monad m => MonadReader r (ReaderT r m)
instance (Monoid w, Monad m) => MonadWriter w (WriterT w m)

In terms of generality the mtl library is the most common general interface for
these monads, which itself depends on the transformers library which generalizes
the "basic" monads described above into transformers.

At their core monad transformers allow us to nest monadic computations in a stack
with an interface to exchange values between the levels,

lift :: (Monad m, MonadTrans t) => m a -> t m a
liftIO :: MonadIO m => IO a -> m a

class MonadTrans t where
    lift :: Monad m => m a -> t m a

class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id

Laws:
#1: lift . return   == return
 OR lift (return x) == return x
#2: lift (m >>= f)  == lift m >>= (lift . f)
 OR do x <- lift m
        lift (f x)
    == lift $ do x <- m
                f x

-}

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

-- examples
-- transformers are composed outside-in
-- e.g. State within Writer within Parser
newtype Parser a = Parser {
                    unParser :: WriterT [Int] (StateT String []) a
                }
-- transformers are unrolled inside out
-- e.g. parse first then write then state
runParser p s = runStateT ( runWriterT ( unParser p ) ) s

{-
There are 3 possible forms of the Reader monad:
1. Reader   (historical haskell 98 version)
    now redifined in terms of MonadReader
2. ReaderT  (transformer variant)
3. MonadReader (mtl variant)
    uses ReaderT

-}

