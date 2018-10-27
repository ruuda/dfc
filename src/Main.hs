import Control.Monad (mapM_)

import Types (Variable, Value (..), Term (..), Field (..), Expr (..), Bindings, newBindings, bind)
import Program (Program)

import qualified Program as Prog

pattern CString str = Const (ValString str)
pattern CInt i = Const (ValInt i)
pattern CTrue = Const (ValBool True)
pattern CFalse = Const (ValBool False)

program :: Program String String
program = Prog.genProgram $ do
  input <- Prog.loadInput
  a <- Prog.add [CInt 0, CInt 1, CInt 2]
  b <- Prog.and [CTrue, CTrue, CFalse]
  c <- Prog.or [b, CTrue, CFalse]
  d <- Prog.add [a, CInt 0, CInt 7]
  name <- Prog.loadField (Field "name" :: Field String)
  title <- Prog.loadField (Field "title" :: Field String)
  Prog.discardIf c
  cat <- Prog.concat [name, CString " ", title]
  cat2 <- Prog.concat [cat, input]
  newName <- Prog.select b (CString "true") cat
  Prog.discardIf b
  pure newName

main :: IO ()
main =
  putStrLn $ show program
