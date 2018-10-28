module Program
  ( Program (..)
  , Gen
  , genProgram
  , add
  , and
  , concat
  , define
  , deref
  , derefInner
  , discardIf
  , loadField
  , loadInput
  , not
  , or
  , select
  ) where

import Prelude hiding (and, concat, or, not)
import Data.List (intercalate)

import Types (Variable, Bindings, Term (..), Tag (..), Deref (..), Expr (..), Field, newBindings, bind)

import qualified Types

data Program a b = Program
  { programInput :: Variable a
  , programBindings :: !Bindings
  , programCondition :: !(Term Bool)
  , programYield :: !(Term b)
  }

instance Show (Program a b) where
  show p = intercalate "\n"
    [ "input " ++ (show $ programInput p)
    , show $ programBindings p
    , "yield " ++ (show $ programYield p) ++ " if " ++ (show $ programCondition p)
    ]

data GenProgram i = GenProgram
  { genInput :: Variable i
  , genBindings :: !Bindings
  , genCondition :: !(Term Bool)
  }

newtype Gen i a = Gen
  { runGen :: GenProgram i -> (a, GenProgram i)
  } deriving Functor

instance Applicative (Gen i) where
  pure x = Gen $ \state -> (x, state)
  genF <*> genV = Gen $ \state ->
    let
      (f, state') = runGen genF state
      (v, state'') = runGen genV state'
    in
      (f v, state'')

instance Monad (Gen i) where
  genV >>= f = Gen $ \state ->
    let
      (v, state') = runGen genV state
    in
      runGen (f v) state'

-- TODO: Make it generic over the input type.
genProgram :: Tag a () -> Gen a (Term b) -> Program a b
genProgram inputTypeTag gen =
  let
    (input, bindings) = newBindings $ fmap (const 0) inputTypeTag
    initial = GenProgram
      { genInput = input
      , genBindings = bindings
      , genCondition = Const (TagBool True)
      }
    (finalValue, finalState) = runGen gen initial
  in
    Program
      { programInput = genInput finalState
      , programBindings = genBindings finalState
      , programCondition = genCondition finalState
      , programYield = finalValue
      }

loadInput :: Gen i (Term i)
loadInput = Gen $ \state -> (Var $ genInput state, state)

define :: Expr Term a -> Gen i (Term a)
define expr = Gen $ \state ->
  let
    (var, bindings) = bind expr $ genBindings state
    newState = state { genBindings = bindings }
  in
    (Var var, newState)

deref :: Term a -> Gen i (Deref Term a)
deref term = Gen $ \state ->
  (Types.deref (genBindings state) term, state)

derefInner :: Expr Term a -> Gen i (Expr (Deref Term) a)
derefInner expr = Gen $ \state ->
  (Types.derefInner (genBindings state) expr, state)

discardIf :: Term Bool -> Gen i ()
discardIf discardCond =
  let
    setCondition cond = Gen $ \state -> ((), state { genCondition = cond })
    getCondition = Gen $ \state -> (genCondition state, state)
  in do
    oldCond <- getCondition
    keepIf <- define $ Not discardCond
    newCond <- define $ And [oldCond, keepIf]
    setCondition newCond

not :: Term Bool -> Gen i (Term Bool)
not = define . Not

and :: [Term Bool] -> Gen i (Term Bool)
and = define . And

or :: [Term Bool] -> Gen i (Term Bool)
or = define . Or

concat :: [Term String] -> Gen i (Term String)
concat = define . Concat

add :: [Term Int] -> Gen i (Term Int)
add = define . Add

loadField :: Field String -> Gen i (Term String)
loadField = define . LoadString

select :: Term Bool -> Term a -> Term a -> Gen i (Term a)
select cond vtrue vfalse = define $ Select cond vtrue vfalse
