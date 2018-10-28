import Types (Variable, Term (..), Tag (..), Field (..), Expr (..), Bindings, newBindings, bind)
import Program (Program)

import qualified Program as Prog

pattern CString str = Const (TagString str)
pattern CInt i = Const (TagInt i)
pattern CTrue = Const (TagBool True)
pattern CFalse = Const (TagBool False)

program :: Program String String
program = Prog.genProgram (TagString ()) $ do
  input <- Prog.loadInput
  a <- Prog.add [CInt 0, CInt 1, CInt 2]
  b <- Prog.and [CTrue, CTrue, CFalse]
  c <- Prog.or [b, CTrue, CFalse]
  _d <- Prog.add [a, CInt 0, CInt 7]
  name <- Prog.loadField (Field "name" :: Field String)
  title <- Prog.loadField (Field "title" :: Field String)
  Prog.discardIf c
  cat <- Prog.concat [name, CString " ", title]
  _cat2 <- Prog.concat [cat, input]
  newName <- Prog.select b (CString "true") cat
  Prog.discardIf b
  pure newName

main :: IO ()
main =
  putStrLn $ show program
