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
  , mapBindings
  , mapMBindings
  , mapExpr
  , unionBindings
  ) where

import Data.List (intercalate)
import Data.IntMap.Strict (IntMap)
import Prelude hiding (lookup)

import qualified Data.IntMap.Strict as IntMap

data Tag a b where
  TagInt    :: b -> Tag Int b
  TagString :: b -> Tag String b
  TagBool   :: b -> Tag Bool b

deriving instance Eq b => Eq (Tag a b)
deriving instance Functor (Tag a)
deriving instance Foldable (Tag a)
deriving instance Traversable (Tag a)

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
  Id         :: t a        -> Expr t a
  Not        :: t Bool     -> Expr t Bool
  And        :: [t Bool]   -> Expr t Bool
  Or         :: [t Bool]   -> Expr t Bool
  Concat     :: [t String] -> Expr t String
  Add        :: [t Int]    -> Expr t Int
  Sub        :: [t Int]    -> Expr t Int
  LoadString :: Field String -> Expr t String
  EqString   :: t String -> t String -> Expr t Bool
  Select     :: t Bool -> t a -> t a -> Expr t a

deriving instance Eq a => Eq (Expr Variable a)

instance Show (Expr Variable a) where
  show op =
    let
      display prefix args = prefix ++ " " ++ (intercalate " " $ fmap show args)
    in case op of
      Const value      -> show value
      Id arg           -> show arg
      Not arg          -> display "not" [arg]
      And args         -> display "and" args
      Or args          -> display "or" args
      Concat args      -> display "concat" args
      Add args         -> display "add" args
      Sub args         -> display "sub" args
      LoadString field -> display "load" [field]
      EqString x y     -> display "eq" [x, y]
      Select cond vtrue vfalse -> "select " ++ (show cond) ++ " " ++ (show vtrue) ++ " " ++ (show vfalse)

data Some f = forall a. Some (f a)

-- Must be a newtype oddly enough, otherwise GHC can't typecheck "Bindings",
-- claiming that TaggedExpr needs a type parameter, even though Some is applied
-- to it.
newtype TaggedExpr a = TaggedExpr (Tag a (Expr Variable a)) deriving (Eq)

instance Eq (Some TaggedExpr) where
  (Some (TaggedExpr ta)) == (Some (TaggedExpr tb)) = case (ta, tb) of
    (TagInt a, TagInt b) -> a == b
    (TagString a, TagString b) -> a == b
    (TagBool a, TagBool b) -> a == b
    _ -> False

-- Tracks bindings, and the counter for the next fresh variable.
data Bindings = Bindings
  { _bindingsFresh :: !Int
  , _bindingsBindings :: !(IntMap (Some TaggedExpr))
  }

deriving instance Eq Bindings

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
      Const value     -> (fmap (const i) value, fmap (const expr) value)
      Id (Variable r) -> (fmap (const i) r, fmap (const expr) r)
      Not {}          -> (TagBool i, TagBool expr)
      And {}          -> (TagBool i, TagBool expr)
      Or {}           -> (TagBool i, TagBool expr)
      Concat {}       -> (TagString i, TagString expr)
      Add {}          -> (TagInt i, TagInt expr)
      Sub {}          -> (TagInt i, TagInt expr)
      LoadString {}   -> (TagString i, TagString expr)
      EqString {}     -> (TagBool i, TagBool expr)
      Select _ (Variable vtrue) _ ->
        (fmap (const i) (vtrue), fmap (const expr) vtrue)
  in
    (Variable var, Bindings (1 + getTag var) (IntMap.insert i (Some $ TaggedExpr val) b))

mapExpr :: (forall b. t b -> u b) -> Expr t a -> Expr u a
mapExpr f expr = case expr of
  Const value -> Const value
  Id ref      -> Id $ f ref
  Not arg     -> Not $ f arg
  And args    -> And $ fmap f args
  Or args     -> Or $ fmap f args
  Concat args -> Concat $ fmap f args
  Add args    -> Add $ fmap f args
  Sub args    -> Sub $ fmap f args
  LoadString field -> LoadString field
  EqString x y -> EqString (f x) (f y)
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

mapBindings :: (forall a. Expr Variable a -> Expr Variable a) -> Bindings -> Bindings
mapBindings f (Bindings i b) = Bindings i (fmap mapBinding b)
  where
    mapBinding (Some (TaggedExpr expr)) = Some $ TaggedExpr $ fmap f expr

mapMBindings
  :: Monad m
  => (forall a. Expr Variable a -> m (Expr Variable a))
  -> Bindings
  -> m Bindings
mapMBindings f (Bindings i b) =
  let
    mapBinding (Some (TaggedExpr expr)) = Some . TaggedExpr <$> (mapM f expr)
  in do
    bindings <- mapM mapBinding b
    pure $ Bindings i bindings

-- Union two bindings with left-bias on duplicate bindings.
unionBindings :: Bindings -> Bindings -> Bindings
unionBindings (Bindings i0 b0) (Bindings i1 b1) =
  Bindings (max i0 i1) (IntMap.union b0 b1)
