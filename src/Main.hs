import Control.Monad (mapM_)
import Data.List (intercalate)

import Types (Variable, Value (..), Term (..), Field (..), Expr (..), Bindings, newBindings, bind)

pattern CString str = Const (ValString str)
pattern CInt i = Const (ValInt i)
pattern CTrue = Const (ValBool True)
pattern CFalse = Const (ValBool False)

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

genProgram :: Gen a (Term b) -> Program a b
genProgram gen =
  let
    (input, bindings) = newBindings
    initial = GenProgram
      { genInput = input
      , genBindings = bindings
      , genCondition = CTrue
      }
    (finalValue, finalState) = runGen gen initial
  in
    Program
      { programInput = genInput finalState
      , programBindings = genBindings finalState
      , programCondition = genCondition finalState
      , programYield = finalValue
      }

define :: Expr a -> Gen i (Term a)
define expr = Gen $ \state ->
  let
    (var, newBindings) = bind expr $ genBindings state
    newState = state { genBindings = newBindings }
  in
    (Var var, newState)

loadInput :: Gen i (Term i)
loadInput = Gen $ \state -> (Var $ genInput state, state)

setCondition :: Term Bool -> Gen i ()
setCondition cond = Gen $ \state -> ((), state { genCondition = cond })

getCondition :: Gen i (Term Bool)
getCondition = Gen $ \state -> (genCondition state, state)

discardIf :: Term Bool -> Gen i ()
discardIf cond = do
  oldCond <- getCondition
  keepIf <- define $ Not cond
  newCond <- define $ And [oldCond, keepIf]
  setCondition newCond

program :: Program String String
program = genProgram $ do
  input <- loadInput
  a <- define $ Add [CInt 0, CInt 1, CInt 2]
  b <- define $ And [CTrue, CTrue, CFalse]
  c <- define $ Or [b, CTrue, CFalse]
  d <- define $ Add [a, CInt 0, CInt 7]
  name <- define $ Load (Field "name" :: Field String)
  title <- define $ Load (Field "title" :: Field String)
  discardIf c
  cat <- define $ Concat [name, CString " ", title]
  cat2 <- define $ Concat [cat, input]
  newName <- define $ Select b (CString "true") cat
  discardIf b
  pure newName

main :: IO ()
main =
  putStrLn $ show program
