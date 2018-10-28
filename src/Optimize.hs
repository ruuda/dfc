module Optimize
  ( optimize
  ) where

import Types (Expr (..), Deref (..), Tag (..))
import Program (Gen, Program)

import qualified Program as Prog

pattern CTrue :: Expr t Bool
pattern CTrue = Const (TagBool True)

pattern CFalse :: Expr t Bool
pattern CFalse = Const (TagBool False)

pattern CInt :: Int -> Expr t Int
pattern CInt i = Const (TagInt i)

pattern CString :: String -> Expr t String
pattern CString str = Const (TagString str)

optimize :: Program a b -> Program a b
optimize = undefined
