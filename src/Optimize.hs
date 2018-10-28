module Optimize
  ( optimize
  ) where

import Data.Foldable (foldr')

import Types (Expr (..), Deref (..), Tag (..), Variable, Value, mapBindings, mapExpr)
import Program (Gen, Program (..))

import qualified Types
import qualified Program as Prog

pattern CTrue :: Expr t Bool
pattern CTrue = Const (TagBool True)

pattern CFalse :: Expr t Bool
pattern CFalse = Const (TagBool False)

pattern CInt :: Int -> Expr t Int
pattern CInt i = Const (TagInt i)

pattern CString :: String -> Expr t String
pattern CString str = Const (TagString str)

pattern DConst :: Value a -> Deref t a
pattern DConst value = DefExpr (Const value)

pattern DNot :: t Bool -> Deref t Bool
pattern DNot expr = DefExpr (Not expr)

pattern DAnd :: [t Bool] -> Deref t Bool
pattern DAnd expr = DefExpr (And expr)

pattern DOr :: [t Bool] -> Deref t Bool
pattern DOr expr = DefExpr (Or expr)

pattern DAdd :: [t Int] -> Deref t Int
pattern DAdd expr = DefExpr (Add expr)

pattern DConcat :: [t String] -> Deref t String
pattern DConcat expr = DefExpr (Concat expr)

pattern VTrue :: Value Bool
pattern VTrue = TagBool True

pattern VFalse :: Value Bool
pattern VFalse = TagBool False

type DoDeref = forall a. Variable a -> Deref Variable a

optimize :: Program a b -> Program a b
optimize program =
  let
    oldBindings = programBindings program
    optimizeExpr :: forall c. Expr Variable c -> Expr Variable c
    optimizeExpr
      = eliminateId (Types.deref oldBindings)
      . rewriteExpr (Types.deref oldBindings)
    newBindings = mapBindings optimizeExpr oldBindings
  in
    program { programBindings = newBindings }

-- Apply rewrite rules that eliminate operations.
rewriteExpr :: DoDeref -> Expr Variable b -> Expr Variable b
rewriteExpr deref expr = case expr of
  Not (deref -> DConst VTrue) -> Const VFalse
  Not (deref -> DConst VFalse) -> Const VTrue
  Not (deref -> DNot x) -> Id x
  And []  -> Const VTrue
  And [x] -> Id x
  And xs  -> rewriteAnd deref xs
  Or []  -> Const VFalse
  Or [x] -> Id x
  Or xs  -> rewriteOr deref xs
  Add []  -> Const (TagInt 0)
  Add [x] -> Id x
  Add xs  -> rewriteAdd deref xs
  Concat []  -> Const (TagString "")
  Concat [x] -> Id x
  Concat xs  -> rewriteConcat deref xs
  Select (deref -> DConst VTrue) vtrue _ -> Id vtrue
  Select (deref -> DConst VFalse) _ vfalse -> Id vfalse
  _ -> expr

rewriteAnd :: DoDeref -> [Variable Bool] -> Expr Variable Bool
rewriteAnd deref = foldr' f (And [])
  where
    f (deref -> DConst VFalse) _  = Const VFalse
    f (deref -> DConst VTrue) z   = z
    -- We inline nested ands even if this is not the sole consumer, under the
    -- assumption that ands are cheap, and this unlocks further optimizations.
    f (deref -> DAnd ys) z = foldr' f z ys
    f x (And xs) = And (x : xs)
    f _ z        = z

rewriteOr :: DoDeref -> [Variable Bool] -> Expr Variable Bool
rewriteOr deref = foldr' f (Or [])
  where
    f (deref -> DConst VTrue) _  = Const VTrue
    f (deref -> DConst VFalse) z = z
    -- We inline nested ors even if this is not the sole consumer, under the
    -- assumption that ors are cheap, and this unlocks further optimizations.
    f (deref -> DOr ys) z = foldr' f z ys
    f x (Or xs)  = Or (x : xs)
    f _ z        = z

rewriteConcat :: DoDeref -> [Variable String] -> Expr Variable String
rewriteConcat deref = Concat . foldr' f []
  where
    f (deref -> DConst (TagString "")) xs = xs
    -- TODO: We should only inline concats if this is the sole consumer.
    f (deref -> DConcat ys) xs = foldr' f xs ys
    f x xs = x : xs

rewriteAdd :: DoDeref -> [Variable Int] -> Expr Variable Int
rewriteAdd deref = Add . foldr' f []
  where
    f (deref -> DConst (TagInt 0)) xs = xs
    -- TODO: We should only inline adds if this is the sole consumer.
    f (deref -> DAdd ys) xs = foldr' f xs ys
    f x xs = x : xs

-- Replace references to an identity expression with references to the source of
-- the identity expression. The replacement follows all the way through to the
-- expression that is not an identity expression.
eliminateId :: DoDeref -> Expr Variable b -> Expr Variable b
eliminateId deref =
  let
    deepSource :: forall c. Variable c -> Variable c
    deepSource var = case deref var of
      DefExpr (Id source) -> deepSource source
      _ -> var
  in
    mapExpr deepSource
