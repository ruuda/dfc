module Types
  ( Variable -- Constructor not exposed to make Gen type safe.
  , Value (..)
  , Term (..)
  , Field (..)
  , Expr (..)
  , mapTerms
  , Bindings
  , newBindings
  , bind
  , mapBindings
  ) where

import Data.List (intercalate)
import Data.IntMap (IntMap)

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

-- A named field.
data Field a where
  Field :: String -> Field a

deriving instance Eq (Field a)

instance Show (Field a) where
  show (Field name) = show name

data Expr a where
  Not    :: Term Bool     -> Expr Bool
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
      Not arg     -> display "not" [arg]
      And args    -> display "and" args
      Or args     -> display "or" args
      Concat args -> display "concat" args
      Add args    -> display "add" args
      Sub args    -> display "sub" args
      Load field  -> display "load" [field]
      Select cond vtrue vfalse -> "select " ++ (show cond) ++ " " ++ (show vtrue) ++ " " ++ (show vfalse)

mapTerms :: (forall a. Term a -> Term a) -> Expr b -> Expr b
mapTerms f expr = case expr of
  Not arg     -> Not $ f arg
  And args    -> And $ fmap f args
  Or args     -> Or $ fmap f args
  Concat args -> Concat $ fmap f args
  Add args    -> Add $ fmap f args
  Sub args    -> Sub $ fmap f args
  Load field  -> Load field
  Select cond vtrue vfalse -> Select (f cond) (f vtrue) (f vfalse)

data Some f = forall a. Some (f a)

-- Tracks bindings, and the counter for the next fresh variable.
data Bindings = Bindings
  { bindingsFresh :: !Int
  , bindingsBindings :: !(IntMap (Some Expr))
  }

instance Show Bindings where
  show (Bindings _ b) =
    let
      showBinding :: (Int, Some Expr) -> String
      showBinding (k, Some v) = (show $ Variable k) ++ " = " ++ (show v)
    in
      intercalate "\n" $ fmap showBinding $ IntMap.toList b

newBindings :: (Variable a, Bindings)
newBindings = (Variable 0, Bindings 1 IntMap.empty)

bind :: Expr a -> Bindings -> (Variable a, Bindings)
bind expr (Bindings i b) = (Variable i, Bindings (i + 1) (IntMap.insert i (Some expr) b))

mapBindings :: (forall a. Expr a -> Expr a) -> Bindings -> Bindings
mapBindings f (Bindings i b) = Bindings i $ fmap (\(Some x) -> Some (f x)) b

inspect :: Variable a -> Bindings -> Some Expr
inspect (Variable i) (Bindings _ b) =
  case IntMap.lookup i b of
    Just expr -> expr
    Nothing -> error "Programming error: looking up unbound variable."
