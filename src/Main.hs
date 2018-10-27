{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

import Control.Monad (mapM_)
import Data.IntMap (IntMap)
import Data.List (intercalate)

import qualified Data.IntMap as IntMap

data Variable a where
  -- A variable is identified by its sequence number.
  Variable :: Int -> Variable a
  deriving (Eq)

instance Show (Variable a) where
  show (Variable i) =
    let
      -- Pick one ANSI color per variable, so we can look at the pretty printed
      -- code and easily identify which variables are the same ones.
      color = (i `mod` 6) + 1
    in
      "\x1b[3" ++ (show color) ++ "m$" ++ (show i) ++ "\x1b[0m"

data Value a where
  ValString :: String -> Value String
  ValInt    :: Int    -> Value Int
  ValBool   :: Bool   -> Value Bool

deriving instance Eq (Value a)

instance Show (Value a) where
  show v = case v of
    ValString str -> show str
    ValInt i -> show i
    ValBool b -> show b

data Term a where
  Var   :: Variable a -> Term a
  Const :: Value a    -> Term a

deriving instance Eq (Term a)

instance Show (Term a) where
  show op = case op of
    Var v   -> show v
    Const c -> "\x1b[37m" ++ (show c) ++ "\x1b[0m"

pattern CString str = Const (ValString str)
pattern CInt i = Const (ValInt i)
pattern CTrue = Const (ValBool True)
pattern CFalse = Const (ValBool False)

-- A named field.
data Field a where
  Field :: String -> Field a

deriving instance Eq (Field a)

instance Show (Field a) where
  show (Field name) = show name

data Expr a where
  And    :: [Term Bool]   -> Expr Bool
  Or     :: [Term Bool]   -> Expr Bool
  Concat :: [Term String] -> Expr String
  Add    :: [Term Int]    -> Expr Int
  Sub    :: [Term Int]    -> Expr Int
  Load   :: Field a       -> Expr a
  Select :: Term Bool -> Term a -> Term a -> Expr a

deriving instance Eq (Expr a)

instance Show (Expr a) where
  show op =
    let
      display prefix args = prefix ++ " " ++ (intercalate " " $ fmap show args)
    in case op of
      And args    -> display "and" args
      Or args     -> display "or" args
      Concat args -> display "concat" args
      Add args    -> display "add" args
      Sub args    -> display "sub" args
      Load field  -> display "load" [field]
      Select cond vtrue vfalse -> "select " ++ (show cond) ++ " " ++ (show vtrue) ++ " " ++ (show vfalse)

data Some f = forall a. Some (f a)

data Program a b = Program
  { programInput :: Variable a
  , programBindings :: !(IntMap (Some Expr))
  , programDiscards :: ![Term Bool]
  , programYield :: !(Term b)
  }

instance Show (Program a b) where
  show p =
    let
      showBinding :: (Int, Some Expr) -> String
      showBinding (k, Some v) = (show $ Variable k) ++ " = " ++ (show v)
      bindings = fmap showBinding $ IntMap.toList $ programBindings p
      discards = fmap (("discardIf " ++) . show) $ programDiscards p
    in
      intercalate "\n" $
        [ "input " ++ (show $ programInput p)
        ] ++ bindings ++ discards ++
        [ "yield " ++ (show $ programYield p)
        ]

data GenProgram i = GenProgram
  { genInput :: Variable i
  , genFresh :: !Int
  , genBindings :: !(IntMap (Some Expr))
  , genDiscards :: ![Term Bool]
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
    initial = GenProgram
      { genInput = Variable 0
      , genFresh = 1
      , genBindings = IntMap.empty
      , genDiscards = []
      }
    (finalValue, finalState) = runGen gen initial
  in
    Program
      { programInput = genInput finalState
      , programBindings = genBindings finalState
      , programDiscards = genDiscards finalState
      , programYield = finalValue
      }

define :: Expr a -> Gen i (Term a)
define expr = Gen $ \state ->
  let
    k = genFresh state
    bindings = genBindings state
    state' = state
      { genFresh = k + 1
      , genBindings = IntMap.insert k (Some expr) bindings
      }
  in
    (Var $ Variable k, state')

loadInput :: Gen i (Term i)
loadInput = Gen $ \state -> (Var $ genInput state, state)

program :: Program String String
program = genProgram $ do
  input <- loadInput
  a <- define $ Add [CInt 0, CInt 1, CInt 2]
  b <- define $ And [CTrue, CTrue, CFalse]
  c <- define $ Or [b, CTrue, CFalse]
  d <- define $ Add [a, CInt 0, CInt 7]
  name <- define $ Load (Field "name" :: Field String)
  title <- define $ Load (Field "title" :: Field String)
  cat <- define $ Concat [name, CString " ", title]
  cat2 <- define $ Concat [cat, input]
  newName <- define $ Select b (CString "true") cat
  pure newName

main :: IO ()
main =
  putStrLn $ show program
