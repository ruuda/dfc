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
  c <- Prog.or [b, true, false]
  _d <- Prog.add =<< (a:) <$> mapM Prog.const [TagInt 0, TagInt 7]
  name <- Prog.loadField (Field "name" :: Field String)
  title <- Prog.loadField (Field "title" :: Field String)
  Prog.discardIf c
  space <- Prog.const $ TagString " "
  cat <- Prog.concat [name, space, title]
  _cat2 <- Prog.concat [cat, input]
  strTrue <- Prog.const $ TagString "true"
  newName <- Prog.select b strTrue cat
  Prog.discardIf b
  false' <- Prog.not true
  Prog.discardIf false'
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
