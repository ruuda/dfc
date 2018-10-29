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
  , deduplicateBindings
  , removeUnusedBindings
  , mapExpr
  , unionBindings
  ) where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.List (intercalate)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Prelude hiding (lookup)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

data Tag a b where
  TagInt    :: b -> Tag Int b
  TagString :: b -> Tag String b
  TagBool   :: b -> Tag Bool b

deriving instance Eq b => Eq (Tag a b)
deriving instance Functor (Tag a)
deriving instance Foldable (Tag a)
deriving instance Traversable (Tag a)

-- instance Hashable b => Hashable (Tag a b) where
--   hashWithSalt salt tag = case tag of
--     TagInt x    -> hashWithSalt (salt + 0) x
--     TagString x -> hashWithSalt (salt + 1) x
--     TagBool x   -> hashWithSalt (salt + 2) x

instance Ord b => Ord (Tag a b) where
  compare p q = case (p, q) of
    (TagInt x, TagInt y)       -> compare x y
    (TagString x, TagString y) -> compare x y
    (TagBool x, TagBool y)     -> compare x y

getTag :: Tag a b -> b
getTag tag = case tag of
  TagInt x    -> x
  TagString x -> x
  TagBool x   -> x

-- Forget the payload but discrimate the tag. Used to implement Ord.
getTagIndex :: Tag a b -> Int
getTagIndex tag = case tag of
  TagInt _    -> 0
  TagString _ -> 1
  TagBool _   -> 2

-- A variable is identified by its sequence number.
newtype Variable a = Variable (Tag a Int)

deriving instance Eq (Variable a)
deriving instance Ord (Variable a)

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
deriving instance Ord (Field a)

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
  Less       :: t Int -> t Int -> Expr t Bool
  Select     :: t Bool -> t a -> t a -> Expr t a

deriving instance Eq a => Eq (Expr Variable a)
deriving instance Ord a => Ord (Expr Variable a)

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
      Less x y         -> display "lt" [x, y]
      Select cond vtrue vfalse -> "select " ++ (show cond) ++ " " ++ (show vtrue) ++ " " ++ (show vfalse)

data Some f = forall a. Some (f a)

-- Must be a newtype oddly enough, otherwise GHC can't typecheck "Bindings",
-- claiming that TaggedExpr needs a type parameter, even though Some is applied
-- to it.
newtype TaggedExpr a = TaggedExpr (Tag a (Expr Variable a)) deriving (Eq, Ord)

instance Eq (Some TaggedExpr) where
  (Some (TaggedExpr ta)) == (Some (TaggedExpr tb)) = case (ta, tb) of
    (TagInt a, TagInt b)       -> a == b
    (TagString a, TagString b) -> a == b
    (TagBool a, TagBool b)     -> a == b
    _ -> False

instance Ord (Some TaggedExpr) where
  compare (Some (TaggedExpr ta)) (Some (TaggedExpr tb)) = case (ta, tb) of
    (TagInt a, TagInt b)       -> compare a b
    (TagString a, TagString b) -> compare a b
    (TagBool a, TagBool b)     -> compare a b
    (a, b) -> compare (getTagIndex a) (getTagIndex b)

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

tagExpr :: forall a. Expr Variable a -> Tag a (Expr Variable a)
tagExpr expr = case expr of
  Const value     -> fmap (const expr) value
  Id (Variable r) -> fmap (const expr) r
  Not {}          -> TagBool expr
  And {}          -> TagBool expr
  Or {}           -> TagBool expr
  Concat {}       -> TagString expr
  Add {}          -> TagInt expr
  Sub {}          -> TagInt expr
  LoadString {}   -> TagString expr
  EqString {}     -> TagBool expr
  Less {}         -> TagBool expr
  Select _ (Variable vtrue) _ -> fmap (const expr) vtrue

-- Erase the type tag from an expression, for storage in an IntMap.
erase :: forall a. Expr Variable a -> Some TaggedExpr
erase = Some . TaggedExpr . tagExpr

bind :: Expr Variable a -> Bindings -> (Variable a, Bindings)
bind expr (Bindings i b) =
  let
    val = tagExpr expr
    var = fmap (const i) val
  in
    (Variable var, Bindings (1 + getTag var) (IntMap.insert i (erase expr) b))

traverseExpr :: Applicative f => (forall b. t b -> f (u b)) -> Expr t a -> f (Expr u a)
traverseExpr f expr = case expr of
  Const value -> pure $ Const value
  Id ref      -> Id <$> f ref
  Not arg     -> Not <$> f arg
  And args    -> And <$> traverse f args
  Or args     -> Or <$> traverse f args
  Concat args -> Concat <$> traverse f args
  Add args    -> Add <$> traverse f args
  Sub args    -> Sub <$> traverse f args
  LoadString field -> pure $ LoadString field
  EqString x y -> EqString <$> f x <*> f y
  Less x y     -> Less <$> f x <*> f y
  Select cond vtrue vfalse -> Select <$> f cond <*> f vtrue <*> f vfalse

mapExpr :: (forall b. t b -> u b) -> Expr t a -> Expr u a
mapExpr f = runIdentity . traverseExpr (Identity . f)

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

-- Replace duplicate bindings with a reference to the first definition. Also
-- known as "Common Subexpression Elimination". We use a Map from expression to
-- variable number, rather than a HashMap, because Ord was mostly derivable and
-- Hashable was not; it is laziness on my part, not a fundamental limitation.
deduplicateBindings :: Bindings -> Bindings
deduplicateBindings (Bindings n b) = Bindings n deduplicated
  where
    deduplicated = State.evalState (IntMap.traverseWithKey dedup b) Map.empty

    -- Replace any expression with an identity expression, referencing variable i.
    reference i (Some (TaggedExpr expr)) = erase $ Id $ Variable $ fmap (const i) expr

    -- Return either the original expression, or an identity expression that
    -- references the variable that first defined that same expression.
    dedup i expr = do
      exprs <- State.get
      case Map.lookup expr exprs of
        Just k  -> pure $ reference k expr
        Nothing -> do
          State.put $ Map.insert expr i exprs
          pure expr

-- Remove all bindings that are not transitive dependencies of the given
-- variable. Also known as "Dead Code Elimination".
removeUnusedBindings :: Variable a -> Bindings -> Bindings
removeUnusedBindings (Variable seed) (Bindings n bs) =
  let
    -- Traverse the dependency graph: start with an open set that contains the
    -- seed and an empty closed set. Move one element from the open set to the
    -- closed set while adding its dependencies to the open set. Recurse until
    -- the open set is empty.
    addVar (Variable k) = State.modify' (IntSet.insert (getTag k)) >> (pure $ Variable k)
    addVars :: forall c. Expr Variable c -> IntSet -> IntSet
    addVars expr = State.execState (traverseExpr addVar expr)
    step closed open = case IntSet.maxView open of
      Nothing -> closed
      Just (i, remaining) -> step (IntSet.insert i closed) $ case IntMap.lookup i bs of
        Nothing -> remaining
        Just (Some (TaggedExpr texpr)) -> addVars (getTag texpr) remaining
    usedVariables = step IntSet.empty $ IntSet.singleton $ getTag seed
  in
    Bindings n $ IntMap.restrictKeys bs usedVariables
