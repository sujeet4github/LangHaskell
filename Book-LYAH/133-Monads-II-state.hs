-- http://learnyouahaskell.com/for-a-few-monads-more#state
--

{-
 a stateful computation is a function that takes some state and returns
 a value along with some new state.

 the state monad handles all this state business for us, without giving up any of the purity
 -}


import Control.Monad.State

{-
http://dev.stephendiehl.com/hask/#monads

The state monad allows functions within a stateful monadic context to access and modify shared state.

runState  :: State s a -> s -> (a, s)
evalState :: State s a -> s -> a
execState :: State s a -> s -> s

-}

test :: State Int Int
test = do
        put 4
        modify (+1)
        get

ex1 = execState test 0

{-
The state monad is often mistakenly described as being impure, but it is in fact entirely pure and the same effect could be achieved by explicitly passing state. A simple implementation of the State monad is only a few lines:

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
  return a = State $ \s -> (a, s)

  State act >>= k = State $ \s ->
    let (a, s') = act s
    in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)

evalState :: State s a -> s -> a
evalState act = fst . runState act

execState :: State s a -> s -> s
execState act = snd . runState act

-}
