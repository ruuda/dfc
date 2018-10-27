{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Monad (mapM_)
import Data.List (intercalate)

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

data Expr a where
  And    :: [Term Bool]   -> Expr Bool
  Or     :: [Term Bool]   -> Expr Bool
  Concat :: [Term String] -> Expr String
  Add    :: [Term Int]    -> Expr Int
  Sub    :: [Term Int]    -> Expr Int

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

data Instr where
  Define :: Variable a -> Expr a -> Instr
  Yield :: Instr

--deriving instance Eq Instr

instance Show Instr where
  show instr = case instr of
    Define var expr -> (show var) ++ " = " ++ (show expr)
    Yield -> "yield"

main :: IO ()
main =
  let
    ops =
      [ Define (Variable 0) (Add [CInt 0, CInt 1, CInt 2])
      , Define (Variable 1) (And [CTrue, CTrue, Var (Variable 0)])
      , Define (Variable 2) (Or [Var (Variable 0), Var (Variable 1), Var (Variable 2)])
      , Define (Variable 3) (Add [Var (Variable 2), Var (Variable 3), Var (Variable 4)])
      ]
  in do
    mapM_ (putStrLn . show) ops
