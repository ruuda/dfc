-- Dataflow Compiler
-- Copyright 2018 Ruud van Asseldonk

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Optimize
  ( optimize
  ) where

import Data.Foldable (foldr')


import Types (Expr (..), Deref (..), Tag (..), Variable, Value)
import Types (mapBindings, deduplicateBindings, removeUnusedBindings, unionBindings, mapExpr)
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

pattern DLess :: t Int -> t Int -> Deref t Bool
pattern DLess x y = DefExpr (Less x y)

pattern DGreaterEq :: t Int -> t Int -> Deref t Bool
pattern DGreaterEq x y = DefExpr (GreaterEq x y)

pattern VTrue :: Value Bool
pattern VTrue = TagBool True

pattern VFalse :: Value Bool
pattern VFalse = TagBool False

type PolyDeref = forall a. Variable a -> Deref Variable a
type PolyRewrite = forall a. Expr Variable a -> Expr Variable a

optimize :: Program a b -> Program a b
optimize =
  let
    dedup p = p { programBindings = deduplicateBindings $ programBindings p }
  in
    -- Five optimization passes (read from back to front):
    -- * Deduplicate (common subexpression eliminaton).
    -- * Rewrite expressions in place.
    -- * Replace usage of identity vars, introduced by deduplicate and by the
    --   previous pass, with their sources.
    -- * A final pass for optimizations that could not be done in-place because
    --   they need to generate new bindings, such as constant folding.
    -- * Dead code elimination.
    eliminateDeadCode
    . optimizeGen
    . eliminateIdExprs
    . rewriteExprs
    . eliminateIdExprs
    . dedup

-- Apply rewriteExpr to all bound expressions.
rewriteExprs :: Program a b -> Program a b
rewriteExprs program =
  let
    bindings = programBindings program
    rewrite :: PolyRewrite
    rewrite = rewriteExpr (Types.deref bindings)
  in
    program { programBindings = mapBindings rewrite bindings }

-- Apply rewrite rules that eliminate operations.
rewriteExpr :: PolyDeref -> Expr Variable b -> Expr Variable b
rewriteExpr deref expr = case expr of
  Not (deref -> DTrue) -> Const VFalse
  Not (deref -> DFalse) -> Const VTrue
  Not (deref -> DNot x) -> Id x
  Not (deref -> DLess x y) -> GreaterEq x y
  Not (deref -> DGreaterEq x y) -> Less x y
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
  Select (deref -> DNot x) vtrue vfalse -> Select x vfalse vtrue
  -- Note: for some reason pattern synonyms don't work here,
  -- we need to spell out DTrue and DFalse in at least one match.
  Select cond (deref -> DConst (TagBool True)) (deref -> DFalse) -> Id cond
  Select cond (deref -> DConst (TagBool False)) (deref -> DTrue) -> Not cond
  EqString x y | x == y -> Const VTrue
  _ -> expr

rewriteAnd :: PolyDeref -> [Variable Bool] -> Expr Variable Bool
rewriteAnd deref = foldr' f (And [])
  where
    f (deref -> DFalse) _ = Const VFalse
    f (deref -> DTrue) z  = z
    -- We inline nested ands even if this is not the sole consumer, under the
    -- assumption that ands are cheap, and this unlocks further optimizations.
    f (deref -> DAnd ys) z = foldr' f z ys
    f x (And xs) = And (x : xs)
    f _ z        = z

rewriteOr :: PolyDeref -> [Variable Bool] -> Expr Variable Bool
rewriteOr deref = foldr' f (Or [])
  where
    f (deref -> DTrue) _  = Const VTrue
    f (deref -> DFalse) z = z
    -- We inline nested ors even if this is not the sole consumer, under the
    -- assumption that ors are cheap, and this unlocks further optimizations.
    f (deref -> DOr ys) z = foldr' f z ys
    f x (Or xs)  = Or (x : xs)
    f _ z        = z

rewriteConcat :: PolyDeref -> [Variable String] -> Expr Variable String
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

rewriteAdd :: PolyDeref -> [Variable Int] -> Expr Variable Int
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
    deref :: PolyDeref
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
deepSource :: forall a. PolyDeref -> Variable a -> Variable a
deepSource deref var = case deref var of
  DefExpr (Id source) -> deepSource deref source
  _ -> var

-- Eliminate usages of variables bound to id expressions. Replace them with
-- references to the sources of those id expressions.
eliminateIdExprs :: Program a b -> Program a b
eliminateIdExprs program =
  let
    bindings = programBindings program
    replace :: forall a. Variable a -> Variable a
    replace = deepSource $ Types.deref bindings
  in
    program
      { programBindings = mapBindings (mapExpr replace) bindings
      , programCondition = replace $ programCondition program
      , programYield = replace $ programYield program
      }

eliminateDeadCode :: Program a b -> Program a b
eliminateDeadCode program =
  let
    -- Remove unused bindings starting from two distinct roots: the yield
    -- variable and the condition variable. Then union to get all live bindings.
    -- It would be more efficient to do it in one pass, but the types make
    -- passing a list of polymorphic seed variables a bit clumsy, so for now
    -- this is easier.
    bindings = programBindings program
    aliveYield = removeUnusedBindings (programYield program) bindings
    aliveCondition = removeUnusedBindings (programCondition program) bindings
    alive = unionBindings aliveYield aliveCondition
  in
    program { programBindings = alive }
