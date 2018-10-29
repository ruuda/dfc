import Control.Monad (void)

import Types (Tag (..), Field (..))
import Program (Program)
import Optimize (optimize)

import qualified Program as Prog

program :: Program String String
program = Prog.genProgram (TagString ()) $ do
  input <- Prog.loadInput
  a <- Prog.add =<< mapM Prog.const [TagInt 0, TagInt 1, TagInt 2]
  b <- Prog.and =<< mapM Prog.const [TagBool True, TagBool True, TagBool False]
  true <- Prog.const $ TagBool True
  false <- Prog.const $ TagBool False
  bb <- Prog.and [true, false, b]
  c <- Prog.or [b, bb, false]
  d <- Prog.add =<< (a:) <$> mapM Prog.const [TagInt 0, TagInt 7]
  level <- Prog.loadField (Field $ TagInt "level")
  levelLess <- Prog.less level d
  Prog.discardIf levelLess
  name <- Prog.loadField $ Field $ TagString "name"
  title <- Prog.loadField $ Field $ TagString "title"
  Prog.discardIf c
  space <- Prog.const $ TagString " "
  cat <- Prog.concat [name, space, title]
  _cat2 <- Prog.concat [cat, input]
  strTrue <- Prog.const $ TagString "true"
  newName <- Prog.select b strTrue cat
  Prog.discardIf b
  false' <- Prog.not true
  Prog.discardIf false'
  include <- Prog.loadField $ Field $ TagString "include"
  shouldInclude <- Prog.eqString include strTrue
  shouldReject <- Prog.not shouldInclude
  Prog.discardIf shouldReject
  strFoobar <- Prog.const $ TagString "foobar"
  strFoo <- Prog.const $ TagString "foo"
  strBar <- Prog.const $ TagString "bar"
  catFoobar <- Prog.concat [strFoo, strBar]
  foobarEqSelf <- Prog.eqString strFoobar catFoobar
  concatWentWrong <- Prog.select foobarEqSelf false true
  Prog.discardIf concatWentWrong
  pure newName

printUntilOptimized :: Program a b -> IO ()
printUntilOptimized p = do
  putStrLn $ show p
  let optimized = optimize p
  if optimized == p
    then putStrLn "\nReached fixed point, done."
    else do
      void $ getLine
      putStrLn "After optimization step:\n"
      printUntilOptimized optimized


main :: IO ()
main = printUntilOptimized program
