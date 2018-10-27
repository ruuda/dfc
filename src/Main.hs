{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
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

-- A named field.
data Field a where
  Field :: String -> Field a

deriving instance Eq (Field a)

instance Show (Field a) where
  show (Field name) = show name

data Expr a where
  And    :: [Term Bool]   -> Expr Bool
  Or     :: [Term Bool]   -> Expr Bool
  Concat :: [Term String] -> Expr String
  Add    :: [Term Int]    -> Expr Int
  Sub    :: [Term Int]    -> Expr Int
  Load   :: Field a       -> Expr a
  Select :: Term Bool -> Term a -> Term a -> Expr a

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
      Load field  -> display "load" [field]
      Select cond vtrue vfalse -> "select " ++ (show cond) ++ " " ++ (show vtrue) ++ " " ++ (show vfalse)

data Instr where
  Define :: Variable a -> Expr a -> Instr
  Yield :: Instr

--deriving instance Eq Instr

instance Show Instr where
  show instr = case instr of
    Define var expr -> (show var) ++ " = " ++ (show expr)
    Yield -> "yield"

data GenState = GenState
  { genStateFresh :: !Int
  , genStateInstrs :: ![Instr]
  }

newtype Gen a = Gen
  { runGen :: GenState -> (a, GenState)
  } deriving Functor

instance Applicative Gen where
  pure x = Gen $ \state -> (x, state)
  genF <*> genV = Gen $ \state ->
    let
      (f, state') = runGen genF state
      (v, state'') = runGen genV state'
    in
      (f v, state'')

instance Monad Gen where
  genV >>= f = Gen $ \state ->
    let
      (v, state') = runGen genV state
    in
      runGen (f v) state'

runGenFull :: Gen () -> [Instr]
runGenFull gen =
  let ((), GenState _ instrs) = runGen gen (GenState 0 []) in reverse instrs

define :: Expr a -> Gen (Variable a)
define expr = Gen $ \state ->
  let
    i = genStateFresh state
    state' = GenState
      { genStateFresh = i + 1
      , genStateInstrs = Define (Variable i) expr : genStateInstrs state
      }
  in
    (Variable i, state')

yield :: Gen ()
yield = Gen $ \state ->
  let
    state' = state { genStateInstrs = Yield : genStateInstrs state }
  in
    ((), state')

program :: Gen ()
program = do
  a <- define $ Add [CInt 0, CInt 1, CInt 2]
  b <- define $ And [CTrue, CTrue, CFalse]
  c <- define $ Or [Var b, CTrue, CFalse]
  d <- define $ Add [Var a, CInt 0, CInt 7]
  name <- define $ Load (Field "name" :: Field String)
  title <- define $ Load (Field "title" :: Field String)
  cat <- define $ Concat [Var name, CString " ", Var title]
  newName <- define $ Select (Var b) (CString "true") (Var cat)
  yield

main :: IO ()
main =
  mapM_ (putStrLn . show) (runGenFull program)
