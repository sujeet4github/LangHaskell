{-
    http://catamorph.de/publications/2004-10-01-monad-transformers.html

Starting from an evaluation function for simple expressions, we convert it to
monadic style and incrementally add error handling, environment passing, state,
logging and input/output by composing monad transformers.

Monads are a remarkably elegant way for structuring programs in a
flexible and extensible way.  They are especially interesting in a
lazy functional language like Haskell, because they allow the
integration of side-effects into otherwise purely functional programs.

Furthermore, by structuring a program with monads, it is possible to
hide much of the necessary book-keeping and plumbing necessary for
many algorithms in a handful of definitions specific for the monad in
use, removing the clutter from the main algorithm.

Monad transformers offer an additional benefit to monadic programming:
by providing a library of different monads and types and functions for
combining these monads, it is possible to create custom monads simply
by composing the necessary monad transformers.

For example, if you need a monad with state and error handling, just
take the |StateT| and |ErrorT| monad transformers and combine them.

-}

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import qualified Data.Map.Lazy as Map


{-
Several of the imported modules beginning with |Control.Monad| are
only needed when you use the monad transformers defined there.

The |Data.Maybe| module defines useful functions for dealing with
optional values of type |Maybe a|, and the module |Data.Map| defines
finite maps.

These will be used to define environments (variable-value mappings) in our
little interpreter.

-}

--------------------------------------------------------------------
------------------------------ Data Structures Used

-- variable names
type Name   = String

-- expressions
data Exp    = Lit Integer
            | Var Name
            | Plus Exp Exp
            | Abs Name Exp
            -- abstractions / lambda expressions
            | App Exp Exp
            deriving (Show, Eq)

-- values
data Value  = IntVal Integer
            -- closure
            | FunVal Env Name Exp
            deriving (Show, Eq)

-- mapping from name to values
type Env    = Map.Map Name Value


--------------------------------------------------------------------
------------------------------ Reference Implementation

-- The interpreter function
-- reference implementation, not monadic
eval0   ::  Env -> Exp -> Value

-- literals evaluate to themselves (packaged in the Value data type)
eval0 env (Lit i)   = IntVal i

-- variables evaluate to the values to which they are bound in the
-- environment
-- fromJust :: Maybe a -> a
-- this introduces error condition - if the variable is not bound
-- program halts
eval0 env (Var n)   = fromJust $ Map.lookup n env

-- Addition is implemented by simply evaluating both operands
-- and returning their sum. Whenever one of the addition operands
-- evaluates to a non-number, the pattern matching in the |let|
-- expression will fail, also terminating the program with an error
-- message.
eval0 env (Plus lexpr rexpr) =  let IntVal lv = eval0 env lexpr
                                    IntVal rv = eval0 env rexpr
                                in
                                    IntVal (lv+rv)

-- Abstractions simply evaluate to functional values, which
-- capture the environment in which they are evaluated.
eval0 env (Abs n e) = FunVal env n e

-- Function application proceeds similar to addition, by first
-- evaluating the function and the argument.
-- The first expression must evaluate to a functional value,
-- whose body is then evaluated in the captured environment,
-- extended with the binding of the function parameter to
-- the argument value.
-- The |case| expression used here to deconstruct the functional
-- value introduces another error possibility.
eval0 env (App l r) =   let lv = eval0 env l
                            rv = eval0 env r
                        in
                            case lv of
                                FunVal env2 n body -> eval0 (Map.insert n rv env2) body

-- In later sections of this text, we will handle these error
-- cases using an error monad, which gives us more control over
-- their handling.

eval0_lit_t1    = (IntVal 10) == calc
    where
        calc = eval0 Map.empty (Lit 10)

eval0_var_t1    = (IntVal 10) == eval0 env (Var "hello")
    where
        env = Map.singleton "hello" (IntVal 10)

eval0_plus_t1    = (IntVal 20) == eval0 env (Plus (Var "hello") (Var "hello"))
    where
        env = Map.singleton "hello" (IntVal 10)
eval0_plus_t2    = (IntVal 15) == eval0 env (Plus (Var "hello") (Lit 5))
    where
        env = Map.singleton "hello" (IntVal 10)

test1_eval0 = eval0_lit_t1
        && eval0_var_t1
        && eval0_plus_t1
        && eval0_plus_t2

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExpE = 12 + ((\x -> x) (4+2))
test2_eval0 = (IntVal exampleExpE) == (eval0 Map.empty exampleExp)

--------------------------------------------------------------------
------------------------------ Monad Transformers

{-
In order to use monad transformers, it is necessary to express
functions in monadic style.  That means that the programmer needs to
impose sequencing on all monadic operations using |do| notation, and
to use the |return| function in order to specify the result of a
function.

-}

{-
|Identity| is a monad imported from |Control.Monad.Identity|, which is
perhaps the simplest monad imaginable:
    it defines the standard |return| and |>>=| operations for
    constructing operations in the monad,
    and additionally a function |runIdentity| to execute such operations.
Other than that, the identity monad has no effect.

In some sense, we will use this monad as a ''base case'', around which
other monad transformers can be wrapped.
-}

-- synonym
type Eval1 a = Identity a
runEval1 :: Eval1 a -> a
runEval1 = runIdentity
-- rewrite |eval0| based on Eval1
eval1   ::  Env -> Exp -> Eval1 Value
eval1 env (Lit i)   = return $ IntVal i
-- maybe function constrains the |Map.lookup| to calling the monad's |fail| function
-- if Nothing is returned - no value is defined in the map
eval1 env (Var n)   = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval1 env (Plus lexpr rexpr) =  do  IntVal lv <- eval1 env lexpr
                                    IntVal rv <- eval1 env rexpr
                                    return $ IntVal (lv+rv)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App l r) =   do  lv <- eval1 env l
                            rv <- eval1 env r
                            case lv of
                                FunVal env2 n body -> eval1 (Map.insert n rv env2) body

test_eval1 = (IntVal exampleExpE) == (runEval1 $ eval1 Map.empty exampleExp)

{-

To recap: conversion to monadic form consists mainly of returning function results
using the |return| function, and sequencing of monadic actions using |do| notation
or the |>>=| or |>>| (monadic bind) functions.

-}

-- Using the Except Monad
type LengthMonad = ExceptT String IO
calculateLength :: LengthMonad Int
calculateLength = do
    -- all the IO operations have to be lifted to the IO monad in the monad stack
    liftIO $ putStrLn "Enter a string? "
    s <- liftIO getLine
    if null s
        then throwError "String was empty!"
        else return $ length s
main2 = do
    -- runExceptT removes the ExceptT wrapper
    r <- runExceptT calculateLength
    reportResult r
-- Prints result of the string length calculation.
reportResult :: Either String Int -> IO ()
reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))

--------------------------------------------------------------------
------------------------------ Error Handling using ExceptT

{-
We have already seen that our evaluation function is partial, that
means it will terminate with an error message for some inputs, for
example for expressions with unbound variables or type errors.

Using monad transformers, we simply go to our local monad transformer
library and take the |ExceptT| monad transformer, using it to extend
the basic |Eval1| monad to |Eval2|.

The |String| type argument to |ExceptT| is the type of exceptions, that
is the values which are used to indicate error conditions.  We use
|String| here to keep things simple, in a real implementation we might
want to include source code locations (in a compiler) or time stamps
(in some kind of web application).

The function for running a computation in the |Eval2| monad changes in
two ways.
First, the result of evaluation is now of type |Either String alpha|,
where the result |Left s| indicates that an error has occurred with
error message |s|, or |Right r|, which stands for successful evaluation
with result~|r|.
Second, we need to call the function |runErrorT| on the given computation
to yield an |Identity| computation, which can in turn be evaluated
using |runIdentity|.

-}

type Eval2 a = ExceptT String Identity a
runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runExceptT

eval2  ::  Env -> Exp -> Eval2 Value
eval2 env (Lit i)   = return $ IntVal i
eval2 env (Var n)   = case Map.lookup n env of
                        Nothing -> throwError ("unbound variable: " ++ n)
                        Just v  -> return v
eval2 env (Plus lexpr rexpr) =  do  lv <- eval2 env lexpr
                                    rv <- eval2 env rexpr
                                    case (lv, rv) of
                                        (IntVal l, IntVal r)    -> return $ IntVal (l+r)
                                        _                       -> throwError "Plus: type error on argument"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App l r) =   do  lv <- eval2 env l
                            rv <- eval2 env r
                            case lv of
                                FunVal env2 n body  -> eval2 (Map.insert n rv env2) body
                                _                   -> throwError "App: type error on left arg"

test_eval2 = Right (IntVal exampleExpE) == (runEval2 $ eval2 Map.empty exampleExp)


--------------------------------------------------------------------
------------- Using ReaderT to provide environment access

{-
Next step: Using Reader Monad

One way to make the definition of the evaluation function even more
pleasing is to hide the environment from all function definitions and
calls.  Since there is only one place where the environment is
extended (for function application) and two places where it is
actually used (for variables and $\lambda$ expressions), we can reduce
the amount of code by hiding it in all other places.

This will be done by adding a |ReaderT| monad transformer in order
to implement a reader monad.  A reader monad passes a value into a
computation and all its sub-computations.

This value can be read by all enclosed computations and get modified
for nested computations.  In contrast to state monads (which will be
introduced in Section~\ref{sec:state-monad}), an encapsulated
computation cannot change the value used by surrounding computations.

In all places where the current environment is needed, it is extracted
from the hidden state of the reader monad using the |ask| function.

In the case of function application, the |local| function is used for
modifying the environment for the recursive call.  Local has the type
|(r -> r) -> m a -> m a|, that is we need to pass in a function which
maps the current environment to the one to be used in the nested
computation, which is the second argument.

In our case, the nested environment does not depend on the current
environment, so we simply pass in a constant function using |const|.

In addition to |ask|, a function |asks| is predefined, which expects
a function mapping the environment to a value.
This can be used to extract individual components of the environment
by applying |asks| to record selector functions.

-}

type Eval3 a = ReaderT Env (ExceptT String Identity) a
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env expr = runIdentity $ runExceptT $ runReaderT expr env

eval3  ::  Exp -> Eval3 Value
eval3 (Lit i)   = return $ IntVal i
eval3 (Var n)   = do
                    env <- ask
                    case Map.lookup n env of
                        Nothing -> throwError ("unbound variable: " ++ n)
                        Just v  -> return v
eval3 (Plus lexpr rexpr) =  do  lv <- eval3 lexpr
                                rv <- eval3 rexpr
                                case (lv, rv) of
                                    (IntVal l, IntVal r)    -> return $ IntVal (l+r)
                                    _                       -> throwError "Plus: type error on argument"
eval3 (Abs n e) = do
                    env <- ask
                    return $ FunVal env n e
eval3 (App l r) =   do  lv <- eval3 l
                        rv <- eval3 r
                        case lv of
                            FunVal env n body  -> local (const (Map.insert n rv env))
                                                    (eval3 body)
                            _                   -> throwError "App: type error on left arg"

test_eval3 = Right (IntVal exampleExpE) == (runEval3 Map.empty $ eval3 exampleExp)


--------------------------------------------------------------------
------------------------------ Adding State

{-
Adding State
------------

Another important application of monads is to provide mutable state to
otherwise purely functional code.  This can be done using a |State|
monad, which provides operations for specifying an initial state,
querying the current state and changing it.

As an example, suppose that we want to add profiling capabilities to
our little interpreter.  We define the new monad by wrapping a |StateT|
constructor around the innermost monad, |Identity|.
(In the case of |State| and |Error| monads, the order of these constructor
matters, as we will see below.)

The state maintained in our example is a simple integer value, but it
could be a value of any data type we wish.  Normally, it will be a record
holding the complete state necessary for the task at hand.

-}
type Eval4 alpha = ReaderT Env (ExceptT String (StateT Integer Identity)) alpha


{-
The return type of the function |runEval4| changes, because the final
state is returned together with the evaluation result (error or
value).  Additionally, we give the initial state as an additional
parameter so that we gain some flexibility (this can be used, for
example, to start a computation in the final state of another one).

-}
runEval4            ::  Env -> Integer -> Eval4 alpha -> (Either String alpha, Integer)
runEval4 env st exp =   runIdentity (runStateT (runExceptT (runReaderT exp env)) st)

-- little helper function |tick|, which gets the hidden state from the
-- computation, increases the counter and stores it back.
tick :: (Num s, MonadState s m) => m ()
tick = do
        st <- get
        put (st + 1)
eval4  ::  Exp -> Eval4 Value
eval4 (Lit i)   = do
                    tick
                    return $ IntVal i
eval4 (Var n)   = do
                    tick
                    env <- ask
                    case Map.lookup n env of
                        Nothing -> throwError ("unbound variable: " ++ n)
                        Just v  -> return v
eval4 (Plus lexpr rexpr) =  do
                                tick
                                lv <- eval4 lexpr
                                rv <- eval4 rexpr
                                case (lv, rv) of
                                    (IntVal l, IntVal r)    -> return $ IntVal (l+r)
                                    _                       -> throwError "Plus: type error on argument"
eval4 (Abs n e) = do
                    tick
                    env <- ask
                    return $ FunVal env n e
eval4 (App l r) =   do
                        tick
                        lv <- eval4 l
                        rv <- eval4 r
                        case lv of
                            FunVal env n body  -> local (const (Map.insert n rv env))
                                                    (eval4 body)
                            _                   -> throwError "App: type error on left arg"

test_eval4 = (Right (IntVal exampleExpE), 8) == (runEval4 Map.empty 0 $ eval4 exampleExp)


{-
The Monad Order
===============
When the type of the |Eval4| monad is changed to the following
(|StateT| and |ErrorT| are swapped), the interpretation of the monad changes.

type Eval4 alpha    = ReaderT Env (ExceptT String (StateT Integer Identity)) alpha
-}
type Eval4' alpha   = ReaderT Env (StateT Integer (ExceptT String Identity)) alpha
{-

Instead of returning a result (error or normal) and a state,
either an error or a result together with the final state is returned,
as can be seen in the type of the corresponding run function:

-}
runEval4'            ::  Env -> Integer -> Eval4' alpha -> (Either String (alpha, Integer))
runEval4' env st ev  =   runIdentity (runExceptT (runStateT (runReaderT ev env) st))
{-
The position of the reader monad transformer does not matter, since it
does not contribute to the final result.

-}


--------------------------------------------------------------------
------------------------------ Writer Monad to add Logging

{-
Similar to |StateT|, |WriterT| interacts with |ExceptT| because it
produces output.  So depending on the order of |ExceptT| and |WriterT|,
the result will include the values written out or not when an error
occurs.  The values to be written out will be lists of strings.  When
you read the documentation for the |WriterT| monad transformer, you
will notice that the type of the output values is restricted to be a
member of the type class |Monoid|.  This is necessary because the
methods of this class are used internally to construct the initial
value and to combine several values written out.
-}


type Eval5 alpha = ReaderT Env  (ExceptT String
                                (WriterT [String] (StateT Integer Identity))) alpha

runEval5            ::  Env -> Integer -> Eval5 alpha -> ((Either String alpha, [String]), Integer)
runEval5 env st ev  =
     runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)

eval5  ::  Exp -> Eval5 Value
eval5 (Lit i)   = do
                    tick
                    return $ IntVal i
eval5 (Var n)   = do
                    tick
                    tell [n]
                    env <- ask
                    case Map.lookup n env of
                        Nothing -> throwError ("unbound variable: " ++ n)
                        Just v  -> return v
eval5 (Plus lexpr rexpr) =  do
                                tick
                                lv <- eval5 lexpr
                                rv <- eval5 rexpr
                                case (lv, rv) of
                                    (IntVal l, IntVal r)    -> return $ IntVal (l+r)
                                    _                       -> throwError "Plus: type error on argument"
eval5 (Abs n e) = do
                    tick
                    env <- ask
                    return $ FunVal env n e
eval5 (App l r) =   do
                        tick
                        lv <- eval5 l
                        rv <- eval5 r
                        case lv of
                            FunVal env n body  -> local (const (Map.insert n rv env))
                                                    (eval5 body)
                            _                   -> throwError "App: type error on left arg"

test_eval5 = ((Right (IntVal exampleExpE),["x"]), 8) == (runEval5 Map.empty 0 $ eval5 exampleExp)

--------------------------------------------------------------------
------------------------------ IO Monad

{-
How do we integrate I/O into the monadic definitions we have
developed so far?  It is not possible to define an I/O monad
transformer, because the execution of I/O operations in Haskell cannot
be arbitrarily nested into other functions or monads, they are only
allowed in the monad |IO|.  Fortunately, the monad transformer library
provides us with the infrastructure to easily integrate I/O operations
into our framework: we simply substitute |IO| where we have used
|Identity|!

This is possible because |Identity| is the base monad,
and as we have seen, the function |runIdentity| for evaluating actions
in this monad is always applied last

The return type of |runEval6| is wrapped in an |IO| constructor, which
means that the running an |Eval6| computation does not directly yield
a result, but an I/O computation which must be run in order to get at
the result.  Accordingly, the |runIdentity| invocation disappears.

-}

type Eval6 alpha = ReaderT Env  (ExceptT String
                                (WriterT [String] (StateT Integer IO))) alpha

runEval6           ::  Env -> Integer -> Eval6 alpha -> IO ((Either String alpha, [String]), Integer)
runEval6 env st ev  =
    runStateT (runWriterT (runExceptT (runReaderT ev env))) st

{-
In the |eval6| function we can now use I/O operations, with one minor
notational inconvenience: we have to invoke the operations using the
function |liftIO|, which lifts the I/O computation into the currently
running monad.  As an example, we chose to print out each integer
constant as soon as it is evaluated.  (We don't think this is good
style, but it illustrates the point and sometimes makes a good
debugging technique.)
-}

eval6               ::  Exp -> Eval6 Value
eval6 (Lit i)   = do
                    tick
                    liftIO $ print i
                    return $ IntVal i
eval6 (Var n)   = do
                    tick
                    tell [n]
                    env <- ask
                    case Map.lookup n env of
                        Nothing -> throwError ("unbound variable: " ++ n)
                        Just v  -> return v
eval6 (Plus lexpr rexpr) =  do
                                tick
                                lv <- eval6 lexpr
                                rv <- eval6 rexpr
                                case (lv, rv) of
                                    (IntVal l, IntVal r)    -> return $ IntVal (l+r)
                                    _                       -> throwError "Plus: type error on argument"
eval6 (Abs n e) = do
                    tick
                    env <- ask
                    return $ FunVal env n e
eval6 (App l r) =   do
                        tick
                        lv <- eval6 l
                        rv <- eval6 r
                        case lv of
                            FunVal env n body  -> local (const (Map.insert n rv env))
                                                    (eval6 body)
                            _                   -> throwError "App: type error on left arg"

--runEval6 Map.empty 0 $ eval6 exampleExp


{-
-}
main = do
        let r0 = eval0 Map.empty exampleExp
        print r0
        let r1 = runEval1 (eval1 Map.empty exampleExp)
        print r1
        let r2 = runEval2 (eval2 Map.empty exampleExp)
        print r2
        let r3 = runEval3 Map.empty (eval3 exampleExp)
        print r3
        let r4 = runEval4 Map.empty 0 (eval4 exampleExp)
        print r4
--        let r4' = runEval4' Map.empty 0 (eval4 exampleExp)
--        print r4'
        let r5 = runEval5 Map.empty 0 (eval5 exampleExp)
        print r5
        let r5' = runEval5 Map.empty 0 (eval5 (Var "x"))
        print r5'
        r6 <- runEval6 Map.empty 0 (eval6 exampleExp)
        print r6
        r6' <- runEval6 Map.empty 0 (eval6 (Var "x"))
        print r6'
