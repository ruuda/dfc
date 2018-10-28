module Optimize
  ( optimize
  ) where

import Data.Foldable (foldr')

import Types (Expr (..), Deref (..), Tag (..), Variable, Value, mapBindings, mapExpr)
import Program (Gen, Program (..))

import qualified Types
import qualified Program as Prog

pattern DConst :: Value a -> Deref t a
pattern DConst value = DefExpr (Const value)

pattern DTrue :: Deref t a
pattern DTrue <- DefExpr (Const (TagBool True))

pattern DFalse :: Deref t a
pattern DFalse <- DefExpr (Const (TagBool False))

-- TODO: Rename CString, for const?
pattern DString :: String -> Deref t a
pattern DString str <- DefExpr (Const (TagString str))

pattern CInt :: Int -> Deref t a
pattern CInt i <- DefExpr (Const (TagInt i))

pattern DNot :: t Bool -> Deref t Bool
pattern DNot expr = DefExpr (Not expr)

pattern DAnd :: [t Bool] -> Deref t Bool
pattern DAnd expr = DefExpr (And expr)

pattern DOr :: [t Bool] -> Deref t Bool
pattern DOr expr = DefExpr (Or expr)

pattern DAdd :: [t Int] -> Deref t Int
pattern DAdd expr = DefExpr (Add expr)

pattern DConcat :: [t String] -> Deref t String
pattern DConcat expr = DefExpr (Concat expr)

pattern VTrue :: Value Bool
pattern VTrue = TagBool True

pattern VFalse :: Value Bool
pattern VFalse = TagBool False

type DoDeref = forall a. Variable a -> Deref Variable a

optimize :: Program a b -> Program a b
optimize program =
  let
    oldBindings = programBindings program
    optimizeExpr :: forall c. Expr Variable c -> Expr Variable c
    optimizeExpr
      = eliminateId (Types.deref oldBindings)
      . rewriteExpr (Types.deref oldBindings)
    newBindings = mapBindings optimizeExpr oldBindings
    rewrittenInPlace = program { programBindings = newBindings }
    regenerated = optimizeGen rewrittenInPlace
  in
    regenerated

-- Apply rewrite rules that eliminate operations.
rewriteExpr :: DoDeref -> Expr Variable b -> Expr Variable b
rewriteExpr deref expr = case expr of
  Not (deref -> DTrue) -> Const VFalse
  Not (deref -> DFalse) -> Const VTrue
  Not (deref -> DNot x) -> Id x
  And []  -> Const VTrue
  And [x] -> Id x
  And xs  -> rewriteAnd deref xs
  Or []  -> Const VFalse
  Or [x] -> Id x
  Or xs  -> rewriteOr deref xs
  Add []  -> Const (TagInt 0)
  Add [x] -> Id x
  Add xs  -> rewriteAdd deref xs
  Concat []  -> Const (TagString "")
  Concat [x] -> Id x
  Concat xs  -> rewriteConcat deref xs
  Select (deref -> DTrue) vtrue _ -> Id vtrue
  Select (deref -> DFalse) _ vfalse -> Id vfalse
  -- Note: for some reason pattern synonyms don't work here,
  -- we need to spell out DTrue and DFalse in at least one match.
  Select cond (deref -> DConst (TagBool True)) (deref -> DFalse) -> Id cond
  Select cond (deref -> DConst (TagBool False)) (deref -> DTrue) -> Not cond
  _ -> expr

rewriteAnd :: DoDeref -> [Variable Bool] -> Expr Variable Bool
rewriteAnd deref = foldr' f (And [])
  where
    f (deref -> DFalse) _ = Const VFalse
    f (deref -> DTrue) z  = z
    -- We inline nested ands even if this is not the sole consumer, under the
    -- assumption that ands are cheap, and this unlocks further optimizations.
    f (deref -> DAnd ys) z = foldr' f z ys
    f x (And xs) = And (x : xs)
    f _ z        = z

rewriteOr :: DoDeref -> [Variable Bool] -> Expr Variable Bool
rewriteOr deref = foldr' f (Or [])
  where
    f (deref -> DTrue) _  = Const VTrue
    f (deref -> DFalse) z = z
    -- We inline nested ors even if this is not the sole consumer, under the
    -- assumption that ors are cheap, and this unlocks further optimizations.
    f (deref -> DOr ys) z = foldr' f z ys
    f x (Or xs)  = Or (x : xs)
    f _ z        = z

rewriteConcat :: DoDeref -> [Variable String] -> Expr Variable String
rewriteConcat deref = Concat . foldr' f []
  where
    f (deref -> DConst (TagString "")) xs = xs
    -- String concat, when implemented as adding the sizes of the parts,
    -- allocating a buffer that large, and memcopying the parts into there, is
    -- dominated by the copies, so if we need "a ++ b" and "a ++ b ++ c", then
    -- computing "a ++ b ++ c" is hardly more expensive than concatenating c to
    -- the result of "a ++ b", so even if the inner concat is used elsewhere and
    -- it could be shared, we still inline it.
    f (deref -> DConcat ys) xs = foldr' f xs ys
    f x xs = x : xs

rewriteAdd :: DoDeref -> [Variable Int] -> Expr Variable Int
rewriteAdd deref = Add . foldr' f []
  where
    f (deref -> DConst (TagInt 0)) xs = xs
    -- TODO: We should only inline adds if this is the sole consumer.
    f (deref -> DAdd ys) xs = foldr' f xs ys
    f x xs = x : xs

-- Apply rewrite optimizations that involve inserting additional expressions,
-- rather than rewriting existing expressions in place. This includes some forms
-- of constant folding, where we need to introduce new constants, for example.
optimizeGen :: forall a b. Program a b -> Program a b
optimizeGen program = Prog.regenProgram rewrite program
  where
    -- Deref always looks up in the previous program, so it does not see
    -- rewrites that may already have occurred. This does not affect semantics,
    -- as rewrites should only rewrite to equivalent expressions anyway. But it
    -- does mean that some optimizations can take more passes to be discovered.
    deref :: DoDeref
    deref = Types.deref (programBindings program)

    rewrite :: forall c. Expr Variable c -> Gen a (Expr Variable c)
    rewrite expr = case expr of
      Concat xs -> Concat <$> f xs
        where
          -- Concatenate two consecutive string constants into one constant.
          f ((deref -> DString x) : (deref -> DString y) : zs) = do
              xCatY <- Prog.const $ TagString (x ++ y)
              f (xCatY : zs)
          f (x : zs) = (x:) <$> f zs
          f [] = pure []
      Add xs -> Add <$> f xs
        where
          -- Concatenate two consecutive integer constants into one constant.
          -- TODO: Should partition into const and non-const and add *all*
          -- constants, not just adjacent ones.
          f ((deref -> CInt x) : (deref -> CInt y) : zs) = do
            xPlusY <- Prog.const $ TagInt (x + y)
            f (xPlusY : zs)
          f (x : zs) = (x:) <$> f zs
          f [] = pure []
      _ -> pure expr

-- Replace references to an identity expression with references to the source of
-- the identity expression. The replacement follows all the way through to the
-- expression that is not an identity expression.
eliminateId :: DoDeref -> Expr Variable b -> Expr Variable b
eliminateId deref =
  let
    deepSource :: forall c. Variable c -> Variable c
    deepSource var = case deref var of
      DefExpr (Id source) -> deepSource source
      _ -> var
  in
    mapExpr deepSource
