{-# LANGUAGE PatternSynonyms #-}

data Variable
  -- A variable is identified by its sequence number.
  = Variable Int
  deriving (Eq)

instance Show Variable where
  show (Variable i) = '$' : show i

data Value
  = VString String
  | VInt Integer
  | VBool Bool
  deriving (Eq, Show)

data Operand
  = Var Variable
  | Const Value
  deriving (Eq, Show)

pattern CString str = Const (VString str)
pattern CInt i = Const (VInt i)
pattern CTrue = Const (VBool True)
pattern CFalse = Const (VBool False)

data Operation
  = And [Operand]
  | Or [Operand]
  | Concat [Operand]
  | Add [Operand]
  | Sub [Operand]
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
