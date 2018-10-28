module Optimize
  ( optimize
  ) where

import Types (Expr (..), Deref (..), Tag (..), Variable, Value, mapBindings)
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

pattern VTrue :: Value Bool
pattern VTrue = TagBool True

pattern VFalse :: Value Bool
pattern VFalse = TagBool False

optimize :: Program a b -> Program a b
optimize program =
  let
    oldBindings = programBindings program
    newBindings = mapBindings (rewriteExpr $ Types.deref oldBindings) oldBindings
  in
    program { programBindings = newBindings }

rewriteExpr :: (t a -> Deref t a) -> Expr t a -> Expr t a
rewriteExpr deref expr = case expr of
  Not (deref -> DConst VTrue) -> Const VFalse
  Not (deref -> DConst VFalse) -> Const VTrue
  _ -> expr
