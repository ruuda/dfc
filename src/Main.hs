{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

data Variable a where
  -- A variable is identified by its sequence number.
  Variable :: Int -> Variable a
  deriving (Eq)

instance Show (Variable a) where
  show (Variable i) = '$' : show i

data Value a where
  VString :: String -> Value String
  VInt    :: Int    -> Value Int
  VBool   :: Bool   -> Value Bool

deriving instance Eq (Value a)
deriving instance Show (Value a)

data Operand a where
  Var   :: Variable a -> Operand a
  Const :: Value a    -> Operand a

deriving instance Eq (Operand a)
deriving instance Show (Operand a)

pattern CString str = Const (VString str)
pattern CInt i = Const (VInt i)
pattern CTrue = Const (VBool True)
pattern CFalse = Const (VBool False)

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
      , And [CTrue, CTrue, CFalse]
      ]
  in do
    putStrLn $ show ops
