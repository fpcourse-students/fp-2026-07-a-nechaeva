-- | Call-by-need normaliser for pure terms.
--
-- The step-by-step evaluator in "Lambda.Eval" rewrites the term tree, so an
-- argument used several times is reduced once per use. A perfectly good
-- solution such as
--
-- > magic    := \t. three (nor (frst t) (scnd t)) (nor (scnd t) (thrd t)) (nor (frst t) (thrd t))
-- > divides3 := \n. thrd (natElim n magic (three false false true))
--
-- then needs about @6^n@ steps and hits any step limit at @n = 9@. Here every
-- argument and every definition is a thunk that is evaluated at most once, so
-- the same term takes a number of steps linear in @n@.
--
-- The result is the β-normal form, the same (up to α) as the normal-order
-- one whenever it exists. Nothing is shown step by step here: chains,
-- @:step@ and @:follow@ keep using "Lambda.Eval".
module Lambda.Need
  ( normalizeNeed
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Map.Strict as Map
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)

import Lambda.Subst (freeVars, fresh)
import Lambda.Syntax

-- | Weak head normal forms: a closure, or a stuck head (a free variable,
-- a literal, a hole) applied to arguments, last argument first.
data Val s
  = VLam Name (Maybe Type) (Env s) Expr
  | VStuck Expr [Thunk s]

type Env s = Map.Map Name (Thunk s)

type Thunk s = STRef s (Cell s)

data Cell s
  = Delayed (Env s) Expr
  | Forced (Val s)
  | BlackHole               -- ^ being forced right now: a cyclic definition

data Stop = OutOfFuel | Unsupported

type M s = ExceptT Stop (ST s)

-- | Normal form of a term under the given definitions, or 'Nothing' if the
-- fuel (β-steps plus nodes of the result) runs out or the term is not pure
-- (contains a meta-level substitution). Definitions may be listed in any
-- order; for a repeated name the first entry wins, as with 'lookup'.
normalizeNeed :: [(Name, Expr)] -> Int -> Expr -> Maybe Expr
normalizeNeed defs fuel0 e0 = runST $ do
  fuel <- newSTRef fuel0
  let defMap = Map.fromList (reverse defs)
  refs <- mapM (const (newSTRef BlackHole)) defMap
  forM_ (Map.toList defMap) $ \(name, body) ->
    forM_ (Map.lookup name refs) $ \ref -> writeSTRef ref (Delayed refs body)
  let taken = concatMap freeVars (e0 : Map.elems defMap)
  result <- runExceptT (eval fuel refs e0 >>= quote fuel taken)
  return (either (const Nothing) Just result)

tick :: STRef s Int -> M s ()
tick fuel = do
  n <- lift (readSTRef fuel)
  if n <= 0 then throwE OutOfFuel else lift (modifySTRef' fuel (subtract 1))

eval :: STRef s Int -> Env s -> Expr -> M s (Val s)
eval fuel env e = case e of
  Var x -> maybe (return (VStuck e [])) (force fuel) (Map.lookup x env)
  Lam x t body -> return (VLam x t env body)
  App f a -> do
    vf <- eval fuel env f
    arg <- delay env a
    apply fuel vf arg
  Lit _ -> return (VStuck e [])
  Hole _ -> return (VStuck e [])
  Subst _ _ -> throwE Unsupported

apply :: STRef s Int -> Val s -> Thunk s -> M s (Val s)
apply fuel v arg = case v of
  VLam x _ env body -> tick fuel >> eval fuel (Map.insert x arg env) body
  VStuck h args -> return (VStuck h (arg : args))

-- | A variable that is already bound needs no new thunk: sharing the old
-- one keeps chains of indirections from building up.
delay :: Env s -> Expr -> M s (Thunk s)
delay env e = case e of
  Var x | Just th <- Map.lookup x env -> return th
  _ -> lift (newSTRef (Delayed env e))

force :: STRef s Int -> Thunk s -> M s (Val s)
force fuel th = do
  cell <- lift (readSTRef th)
  case cell of
    Forced v -> return v
    BlackHole -> throwE Unsupported
    Delayed env e -> do
      lift (writeSTRef th BlackHole)
      v <- eval fuel env e
      lift (writeSTRef th (Forced v))
      return v

-- | Read a value back as a term, normalising under binders. A binder keeps
-- its name unless that name is free in the input or bound further out.
quote :: STRef s Int -> [Name] -> Val s -> M s Expr
quote fuel taken v = do
  tick fuel
  case v of
    VLam x t env body -> do
      let x' = fresh taken x
      var <- lift (newSTRef (Forced (VStuck (Var x') [])))
      inner <- eval fuel (Map.insert x var env) body
      Lam x' t <$> quote fuel (x' : taken) inner
    VStuck h args -> do
      args' <- mapM (\th -> force fuel th >>= quote fuel taken) (reverse args)
      return (apps h args')
