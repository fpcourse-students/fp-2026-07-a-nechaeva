-- | Simple types: unification, principal types (Curry), Church checking.
--
-- Church annotations on binders are rigid: their type variables are
-- treated as constants during inference (they were given by the
-- student), then turned back into ordinary variables in the result.
module Lambda.Types
  ( TypeError (..)
  , prettyTypeError
  , primTypes
  , inferType
  , checkChurch
  , typesEqualUpToRenaming
  , isInstanceOf
  , unify
  , applySubst
  , typeVars
  ) where

import Data.List (nub)

import Lambda.Syntax

data TypeError
  = UnboundVar Name
  | CannotUnify Type Type
  | Occurs Name Type
  | HoleInTerm
  | MissingAnnotation Name
  | NotAllowed String
  deriving (Eq, Show)

-- | Render an error. The printer receives both types of a mismatch
-- at once so that variables are renamed consistently.
prettyTypeError :: (Type -> Type -> (String, String)) -> TypeError -> String
prettyTypeError pp2 err = case err of
  UnboundVar x -> "переменная ‘" ++ x ++ "’ не определена"
  CannotUnify a b -> let (sa, sb) = pp2 a b in "не удаётся унифицировать " ++ sa ++ " и " ++ sb
  Occurs a t -> let (sa, st) = pp2 (TVar a) t in "бесконечный тип: " ++ sa ++ " ∼ " ++ st
  HoleInTerm -> "в терме осталась дырка"
  MissingAnnotation x -> "связыватель ‘" ++ x ++ "’ без аннотации типа"
  NotAllowed what -> what

type Subst = [(Name, Type)]

-- | Types of the primitives of @language typed@.
primTypes :: [(Name, Type)]
primTypes =
  [ ("true", TCon "Bool"), ("false", TCon "Bool")
  , ("plus", ii), ("minus", ii), ("mult", ii)
  , ("eq", ib), ("lt", ib)
  , ("iszero", TArr int bool)
  , ("if", TArr bool (TArr a (TArr a a)))
  ]
  where
    int = TCon "Int"
    bool = TCon "Bool"
    ii = TArr int (TArr int int)
    ib = TArr int (TArr int bool)
    a = TVar "a"

typeVars :: Type -> [Name]
typeVars = nub . go
  where
    go (TVar a) = [a]
    go (TCon _) = []
    go (TArr x y) = go x ++ go y

applySubst :: Subst -> Type -> Type
applySubst s t = case t of
  TVar a -> maybe t (applySubst s) (lookup a s)
  TCon _ -> t
  TArr x y -> TArr (applySubst s x) (applySubst s y)

-- Rigid (skolem) variables are represented as constants with a prefix
-- that ordinary names cannot contain.
rigidPrefix :: String
rigidPrefix = "!"

rigid :: Type -> Type
rigid (TVar a) = TCon (rigidPrefix ++ a)
rigid (TCon c) = TCon c
rigid (TArr x y) = TArr (rigid x) (rigid y)

unrigid :: Type -> Type
unrigid (TCon c) | take 1 c == rigidPrefix = TVar (drop 1 c)
unrigid (TCon c) = TCon c
unrigid (TVar a) = TVar a
unrigid (TArr x y) = TArr (unrigid x) (unrigid y)

-- | Most general unifier, extending the given substitution.
unify :: Subst -> Type -> Type -> Either TypeError Subst
unify s t1 t2 = go (applySubst s t1) (applySubst s t2)
  where
    go (TVar a) (TVar b) | a == b = Right s
    go (TVar a) t = bind a t
    go t (TVar a) = bind a t
    go (TCon a) (TCon b) | a == b = Right s
    go (TArr a1 b1) (TArr a2 b2) = do
      s' <- unify s a1 a2
      unify s' b1 b2
    go a b = Left (CannotUnify (unrigid a) (unrigid b))

    bind a t
      | a `elem` typeVars t = Left (Occurs a (unrigid t))
      | otherwise = Right ((a, t) : s)

-- | Fresh variable names that cannot clash with user names.
freshName :: Int -> Name
freshName i = "?" ++ show i

-- | Principal type (Curry style). Free variables of the term are looked
-- up in the given environment (primitives, or nothing). Annotations
-- are rigid. Type variables in the result are the user's annotation
-- names where they came from annotations, and fresh names otherwise.
inferType :: [(Name, Type)] -> Expr -> Either TypeError Type
inferType prims e0 = do
  (t, s, _) <- go [] [] 0 e0
  Right (unrigid (applySubst s t))
  where
    -- env: λ-bound variables (monomorphic); prims: polymorphic constants
    go env s n expr = case expr of
      Var x
        | Just t <- lookup x env -> Right (t, s, n)
        | Just t <- lookup x prims -> Right (instantiate n t, s, n + length (typeVars t))
        | otherwise -> Left (UnboundVar x)
      Lit _ -> Right (TCon "Int", s, n)
      Hole _ -> Left HoleInTerm
      Subst {} -> Left (NotAllowed "подстановка не является термом")
      Lam x ann body -> do
        let (tx, n1) = case ann of
              Just t  -> (rigid t, n)
              Nothing -> (TVar (freshName n), n + 1)
        (tb, s', n2) <- go ((x, tx) : env) s n1 body
        Right (TArr tx tb, s', n2)
      App f a -> do
        (tf, s1, n1) <- go env s n f
        (ta, s2, n2) <- go env s1 n1 a
        let tr = TVar (freshName n2)
        s3 <- unify s2 tf (TArr ta tr)
        Right (tr, s3, n2 + 1)

    -- Instantiate the type of a primitive with fresh variables.
    instantiate n t =
      let vs = typeVars t
          table = zip vs [ TVar (freshName (n + i)) | i <- [0 ..] ]
      in  applySubst table t

-- | Church typing: every binder must be annotated; the type is computed
-- from the annotations (no guessing). Returns the type of the term.
checkChurch :: [(Name, Type)] -> Expr -> Either TypeError Type
checkChurch prims e0 = do
  (t, s) <- go [] [] e0
  Right (unrigid (applySubst s t))
  where
    go env s expr = case expr of
      Var x
        | Just t <- lookup x env -> Right (t, s)
        | Just t <- lookup x prims -> Right (rigid t, s)
        | otherwise -> Left (UnboundVar x)
      Lit _ -> Right (TCon "Int", s)
      Hole _ -> Left HoleInTerm
      Subst {} -> Left (NotAllowed "подстановка не является термом")
      Lam x Nothing _ -> Left (MissingAnnotation x)
      Lam x (Just t) body -> do
        (tb, s') <- go ((x, rigid t) : env) s body
        Right (TArr (rigid t) tb, s')
      App f a -> do
        (tf, s1) <- go env s f
        (ta, s2) <- go env s1 a
        case applySubst s2 tf of
          TArr targ tres -> do
            s3 <- unify s2 targ ta
            Right (tres, s3)
          other -> Left (CannotUnify (unrigid other) (TArr (unrigid ta) (TVar "?")))

-- | Equal up to a bijective renaming of type variables.
typesEqualUpToRenaming :: Type -> Type -> Bool
typesEqualUpToRenaming a b = case go [] a b of
  Just _ -> True
  Nothing -> False
  where
    go m (TVar x) (TVar y) = case (lookup x m, lookup y (map swap m)) of
      (Nothing, Nothing) -> Just ((x, y) : m)
      (Just y', Just x') | y' == y && x' == x -> Just m
      _ -> Nothing
    go m (TCon x) (TCon y) | x == y = Just m
    go m (TArr a1 b1) (TArr a2 b2) = go m a1 a2 >>= \m' -> go m' b1 b2
    go _ _ _ = Nothing
    swap (x, y) = (y, x)

-- | @isInstanceOf t general@: is @t@ a substitution instance of @general@?
isInstanceOf :: Type -> Type -> Bool
isInstanceOf t general = case match [] general t of
  Just _ -> True
  Nothing -> False
  where
    match m (TVar a) x = case lookup a m of
      Nothing -> Just ((a, x) : m)
      Just x' | x' == x -> Just m
      _ -> Nothing
    match m (TCon c) (TCon d) | c == d = Just m
    match m (TArr a1 b1) (TArr a2 b2) = match m a1 a2 >>= \m' -> match m' b1 b2
    match _ _ _ = Nothing
