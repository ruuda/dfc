module Optimize
  ( optimize
  ) where

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

pattern VTrue :: Value Bool
pattern VTrue = TagBool True

pattern VFalse :: Value Bool
pattern VFalse = TagBool False

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
rewriteExpr :: (forall a. Variable a -> Deref Variable a) -> Expr Variable b -> Expr Variable b
rewriteExpr deref expr = case expr of
  Not (deref -> DConst VTrue) -> Const VFalse
  Not (deref -> DConst VFalse) -> Const VTrue
  Not (deref -> DNot x) -> Id x
  _ -> expr

-- Replace references to an identity expression with references to the source of
-- the identity expression. The replacement follows all the way through to the
-- expression that is not an identity expression.
eliminateId :: (forall a. Variable a -> Deref Variable a) -> Expr Variable b -> Expr Variable b
eliminateId deref =
  let
    deepSource :: forall c. Variable c -> Variable c
    deepSource var = case deref var of
      DefExpr (Id source) -> deepSource source
      _ -> var
  in
    mapExpr deepSource
