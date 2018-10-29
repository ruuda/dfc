module Program
  ( Program (..)
  , Gen
  , genProgram
  , regenProgram
  , add
  , and
  , const
  , concat
  , define
  , deref
  , derefInner
  , discardIf
  , loadField
  , eqString
  , less
  , loadInput
  , not
  , or
  , select
  ) where

import Prelude hiding (and, concat, const, or, not)
import Data.List (intercalate)

import qualified Prelude

import Types (Variable, Bindings, Tag (..), Deref (..), Expr (..), Field, Value,
              newBindings, bind, unionBindings, mapMBindings)

import qualified Types

data Program a b = Program
  { programInput :: Variable a
  , programBindings :: !Bindings
  , programCondition :: !(Variable Bool)
  , programYield :: !(Variable b)
  }

deriving instance Eq (Program a b)

instance Show (Program a b) where
  show p = intercalate "\n"
    [ "input " ++ (show $ programInput p)
    , show $ programBindings p
    , "yield " ++ (show $ programYield p) ++ " if " ++ (show $ programCondition p)
    ]

data GenProgram i = GenProgram
  { genInput :: Variable i
  , genBindings :: !Bindings
  , genCondition :: !(Variable Bool)
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
genProgram :: Tag a () -> Gen a (Variable b) -> Program a b
genProgram inputTypeTag gen =
  let
    (input, bindings) = newBindings $ fmap (Prelude.const 0) inputTypeTag
    (true, bindings') = bind (Const $ TagBool True) bindings
    initial = GenProgram
      { genInput = input
      , genBindings = bindings'
      , genCondition = true
      }
    (finalValue, finalState) = runGen gen initial
  in
    Program
      { programInput = genInput finalState
      , programBindings = genBindings finalState
      , programCondition = genCondition finalState
      , programYield = finalValue
      }

-- Map a potentially code-generating function over all bindings in the program.
regenProgram
  :: forall i b
   . (forall a. Expr Variable a -> Gen i (Expr Variable a))
  -> Program i b
  -> Program i b
regenProgram f program =
  let
    oldBindings = programBindings program
    initial = GenProgram
      { genInput = programInput program
      , genBindings = programBindings program
      , genCondition = programCondition program
      }
    (bindings, finalState) = runGen (mapMBindings f oldBindings) initial
    -- Now we have two sets of bindings: the ones in the final state, modified
    -- by the mapping side effects, and also the mapped bindings. Fortunately
    -- side effects cannot modify exisiting bindings, so we can merge both, with
    -- the mapped ones taking precedence.
    mergedBindings = unionBindings bindings (genBindings finalState)
  in
    -- Regenerating does not change the yield or input -- there is no
    -- possibility to do so. Only the bindings can change, and the condition
    -- may be modified as well.
    program
      { programBindings = mergedBindings
      , programCondition = genCondition finalState
      }

loadInput :: Gen i (Variable i)
loadInput = Gen $ \state -> (genInput state, state)

define :: Expr Variable a -> Gen i (Variable a)
define expr = Gen $ \state ->
  let
    (var, bindings) = bind expr $ genBindings state
    newState = state { genBindings = bindings }
  in
    (var, newState)

deref :: Variable a -> Gen i (Deref Variable a)
deref term = Gen $ \state ->
  (Types.deref (genBindings state) term, state)

derefInner :: Expr Variable a -> Gen i (Expr (Deref Variable) a)
derefInner expr = Gen $ \state ->
  (Types.derefInner (genBindings state) expr, state)

discardIf :: Variable Bool -> Gen i ()
discardIf discardCond =
  let
    setCondition cond = Gen $ \state -> ((), state { genCondition = cond })
    getCondition = Gen $ \state -> (genCondition state, state)
  in do
    oldCond <- getCondition
    keepIf <- define $ Not discardCond
    newCond <- define $ And [oldCond, keepIf]
    setCondition newCond

const :: Value a -> Gen i (Variable a)
const = define . Const

not :: Variable Bool -> Gen i (Variable Bool)
not = define . Not

and :: [Variable Bool] -> Gen i (Variable Bool)
and = define . And

or :: [Variable Bool] -> Gen i (Variable Bool)
or = define . Or

concat :: [Variable String] -> Gen i (Variable String)
concat = define . Concat

add :: [Variable Int] -> Gen i (Variable Int)
add = define . Add

loadField :: Field String -> Gen i (Variable String)
loadField = define . LoadString

eqString :: Variable String -> Variable String -> Gen i (Variable Bool)
eqString a b = define $ EqString a b

less :: Variable Int -> Variable Int -> Gen i (Variable Bool)
less a b = define $ Less a b

select :: Variable Bool -> Variable a -> Variable a -> Gen i (Variable a)
select cond vtrue vfalse = define $ Select cond vtrue vfalse
