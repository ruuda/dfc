{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

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
deriving instance Show (Value a)

data Operand a where
  Var   :: Variable a -> Operand a
  Const :: Value a    -> Operand a

deriving instance Eq (Operand a)
deriving instance Show (Operand a)

pattern CString str = Const (ValString str)
pattern CInt i = Const (ValInt i)
pattern CTrue = Const (ValBool True)
pattern CFalse = Const (ValBool False)

data Operation
  = And [Operand Bool]
  | Or [Operand Bool]
  | Concat [Operand String]
  | Add [Operand Int]
  | Sub [Operand Int]
  deriving (Eq, Show)

main :: IO ()
main =
  let
    ops =
      [ Add [CInt 0, CInt 1, CInt 2]
      , And [CTrue, CTrue, Var (Variable 0)]
      , Or [Var (Variable 0), Var (Variable 1), Var (Variable 2)]
      , Add [Var (Variable 2), Var (Variable 3), Var (Variable 4)]
      ]
  in do
    putStrLn $ show ops
