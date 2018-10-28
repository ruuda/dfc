module Optimize
  ( rewriteDereferenced
  ) where

import Types (Expr (..), Term (..), Deref (..), Tag (..))
import Program (Gen)

import qualified Program as Prog

pattern CTrue :: Term Bool
pattern CTrue = Const (TagBool True)

pattern CFalse :: Term Bool
pattern CFalse = Const (TagBool False)

pattern CInt :: Int -> Term Int
pattern CInt i = Const (TagInt i)

pattern CString :: String -> Term String
pattern CString str = Const (TagString str)

pattern DTrue :: Deref Term Bool
pattern DTrue = DefConst (TagBool True)

pattern DFalse :: Deref Term Bool
pattern DFalse = DefConst (TagBool False)

pattern DInt :: Int -> Deref Term Int
pattern DInt i = DefConst (TagInt i)

pattern DString :: String -> Deref Term String
pattern DString str = DefConst (TagString str)

--pattern x <- DefExpr x

rewriteDereferenced :: Expr Term a -> Gen i (Term a)
rewriteDereferenced expr = do
  dexpr <- Prog.derefInner expr
  case dexpr of
    Not DTrue  -> pure CFalse
    Not DFalse -> pure CTrue
    --Not (Var (deref -> Def (Not x))) -> pure x
    --Not (Var v) -> case deref v of
    --  Def (Not x) -> pure x
    --  _ -> Prog.define expr

    _ -> Prog.define expr

    --  And args    -> display "and" args
    --  Or args     -> display "or" args
    --  Concat args -> display "concat" args
    --  Add args    -> display "add" args
    --  Sub args    -> display "sub" args
    --  Load field  -> display "load" [field]
    --  Select cond vtrue vfalse -> "select " ++ (show cond) ++ " " ++ (show vtrue) ++ " " ++ (show vfalse)
