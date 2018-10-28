module Types
  ( Variable -- Constructor not exposed to make Gen type safe.
  , Value
  , Tag (..)
  , Deref (..)
  , Field (..)
  , Expr (..)
  , Bindings
  , Some (..)
  , newBindings
  , bind
  , deref
  , derefInner
  ) where

import Data.List (intercalate)
import Data.IntMap (IntMap)
import Prelude hiding (lookup)

import qualified Data.IntMap as IntMap

data Tag a b where
  TagInt    :: b -> Tag Int b
  TagString :: b -> Tag String b
  TagBool   :: b -> Tag Bool b

deriving instance Eq b => Eq (Tag a b)
deriving instance Functor (Tag a)

getTag :: Tag a b -> b
getTag tag = case tag of
  TagInt x    -> x
  TagString x -> x
  TagBool x   -> x

-- A variable is identified by its sequence number.
newtype Variable a = Variable (Tag a Int)

deriving instance Eq (Variable a)

instance Show (Variable a) where
  show (Variable tag) =
    let
      i :: Int; ann :: String
      (i, ann) = case tag of
        TagInt k    -> (k, "int")
        TagString k -> (k, "str")
        TagBool k   -> (k, "bit")
      -- Pick one ANSI color per variable, so we can look at the pretty printed
      -- code and easily identify which variables are the same ones.
      color = (i `mod` 6) + 1
    in
      "\x1b[3" ++ (show color) ++ "m$" ++ (show i) ++ ":" ++ ann ++ "\x1b[0m"

type Value a = Tag a a

instance Show (Tag a a) where
  show v = "\x1b[37m" ++ s ++ "\x1b[0m"
    where
      s = case v of
        TagString str -> show str
        TagInt i -> show i
        TagBool True -> "true"
        TagBool False -> "false"

-- A named field.
data Field a where
  Field :: String -> Field a

deriving instance Eq (Field a)

instance Show (Field a) where
  show (Field name) = show name

data Expr t a where
  Const      :: Value a    -> Expr t a
  Not        :: t Bool     -> Expr t Bool
  And        :: [t Bool]   -> Expr t Bool
  Or         :: [t Bool]   -> Expr t Bool
  Concat     :: [t String] -> Expr t String
  Add        :: [t Int]    -> Expr t Int
  Sub        :: [t Int]    -> Expr t Int
  LoadString :: Field String -> Expr t String
  Select     :: t Bool -> t a -> t a -> Expr t a

deriving instance Eq a => Eq (Expr Variable a)

instance Show (Expr Variable a) where
  show op =
    let
      display prefix args = prefix ++ " " ++ (intercalate " " $ fmap show args)
    in case op of
      Const value      -> show value
      Not arg          -> display "not" [arg]
      And args         -> display "and" args
      Or args          -> display "or" args
      Concat args      -> display "concat" args
      Add args         -> display "add" args
      Sub args         -> display "sub" args
      LoadString field -> display "load" [field]
      Select cond vtrue vfalse -> "select " ++ (show cond) ++ " " ++ (show vtrue) ++ " " ++ (show vfalse)

data Some f = forall a. Some (f a)

-- Must be a newtype oddly enough, otherwise GHC can't typecheck "Bindings",
-- claiming that TaggedExpr needs a type parameter, even though Some is applied
-- to it.
newtype TaggedExpr a = TaggedExpr (Tag a (Expr Variable a)) deriving (Eq)

-- Tracks bindings, and the counter for the next fresh variable.
data Bindings = Bindings
  { _bindingsFresh :: !Int
  , _bindingsBindings :: !(IntMap (Some TaggedExpr))
  }

instance Show Bindings where
  show (Bindings _ b) =
    let
      showBinding :: (Int, Some TaggedExpr) -> String
      showBinding (k, Some (TaggedExpr v)) =
        (show $ Variable (fmap (const k) v)) ++ " = " ++ (show $ getTag v)
    in
      intercalate "\n" $ fmap showBinding $ IntMap.toList b

-- Create new bindings, where the initial binding counter starts at the given
-- value and has the tagged type.
newBindings :: Tag a Int -> (Variable a, Bindings)
newBindings tag = (Variable tag, Bindings (1 + getTag tag) IntMap.empty)

bind :: Expr Variable a -> Bindings -> (Variable a, Bindings)
bind expr (Bindings i b) =
  let
    (var, val) = case expr of
      Const value   -> (fmap (const i) value, fmap (const expr) value)
      Not {}        -> (TagBool i, TagBool expr)
      And {}        -> (TagBool i, TagBool expr)
      Or {}         -> (TagBool i, TagBool expr)
      Concat {}     -> (TagString i, TagString expr)
      Add {}        -> (TagInt i, TagInt expr)
      Sub {}        -> (TagInt i, TagInt expr)
      LoadString {} -> (TagString i, TagString expr)
      Select _ (Variable vtrue) _ ->
        (fmap (const i) (vtrue), fmap (const expr) vtrue)
  in
    (Variable var, Bindings (1 + getTag var) (IntMap.insert i (Some $ TaggedExpr val) b))

mapExpr :: (forall b. t b -> u b) -> Expr t a -> Expr u a
mapExpr f expr = case expr of
  Const value -> Const value
  Not arg     -> Not $ f arg
  And args    -> And $ fmap f args
  Or args     -> Or $ fmap f args
  Concat args -> Concat $ fmap f args
  Add args    -> Add $ fmap f args
  Sub args    -> Sub $ fmap f args
  LoadString field -> LoadString field
  Select cond vtrue vfalse -> Select (f cond) (f vtrue) (f vfalse)

data Deref t a where
  DefOpaque :: Deref t a
  DefExpr   :: Expr t a -> Deref t a

deref :: Bindings -> Variable a -> Deref Variable a
deref (Bindings _ b) (Variable v) = case v of
  TagInt i -> case IntMap.lookup i b of
    Just (Some (TaggedExpr (TagInt expr))) -> DefExpr expr
    Just _ -> error "Programming error: type mismatch on deref, expected int."
    Nothing -> DefOpaque
  TagString i -> case IntMap.lookup i b of
    Just (Some (TaggedExpr (TagString expr))) -> DefExpr expr
    Just _ -> error "Programming error: type mismatch on deref, expected string."
    Nothing -> DefOpaque
  TagBool i -> case IntMap.lookup i b of
    Just (Some (TaggedExpr (TagBool expr))) -> DefExpr expr
    Just _ -> error "Programming error: type mismatch on deref, expected bool."
    Nothing -> DefOpaque

derefInner :: Bindings -> Expr Variable a -> Expr (Deref Variable) a
derefInner bindings = mapExpr (deref bindings)
