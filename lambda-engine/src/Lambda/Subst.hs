-- | Free variables, capture-avoiding substitution, α-equivalence,
-- name expansion and a few syntactic checks on terms.
module Lambda.Subst
  ( freeVars
  , unboundIn
  , subst
  , substAll
  , rename
  , fresh
  , hasHole
  , hasSubst
  , alphaEq
  , binders
  , barendregt
  , erase
  , referencedNames
  , namesUsedBy
  , expandAll
  , etaReduce
  , etaContractions
  , parenCount
  ) where

import Data.List (nub)

import Lambda.Syntax

-- | Free (unbound) names in a term, in first-occurrence order.
freeVars :: Expr -> [Name]
freeVars = nub . go []
  where
    go bound (Var x)
      | x `elem` bound = []
      | otherwise      = [x]
    go bound (Lam x _ e) = go (x : bound) e
    go bound (App f a) = go bound f ++ go bound a
    go _     (Hole _)  = []
    go _     (Lit _)   = []
    go bound (Subst bs m) =
      concatMap (go bound . snd) bs ++ go (map fst bs ++ bound) m ++ [ x | (x, _) <- bs, x `notElem` bound ]

-- | Free names that are not defined in the environment.
unboundIn :: [(Name, Expr)] -> Expr -> [Name]
unboundIn env expr = filter (`notElem` map fst env) (freeVars expr)

-- | @subst x s e@ replaces free occurrences of @x@ in @e@ with @s@,
-- renaming binders when needed so that free names of @s@ are not captured.
subst :: Name -> Expr -> Expr -> Expr
subst x s = go
  where
    sFree = freeVars s

    go (Var y)
      | x == y    = s
      | otherwise = Var y
    go hole@(Hole _) = hole
    go lit@(Lit _) = lit
    go (App f a) = App (go f) (go a)
    go (Lam y t body)
      | y == x          = Lam y t body
      | y `notElem` sFree = Lam y t (go body)
      | otherwise =
          let y' = fresh (x : y : sFree ++ freeVars body) y
          in  Lam y' t (go (rename y y' body))
    go (Subst bs m) = Subst [ (y, go n) | (y, n) <- bs ] m  -- meta-level; not expected here

-- | Replace free @old@ with @new@. Uses 'subst', so inner binders that
-- would capture @new@ are renamed automatically.
rename :: Name -> Name -> Expr -> Expr
rename old new = subst old (Var new)

-- | Simultaneous substitution: every variable is first renamed to a
-- private placeholder, so that a replacement term is never
-- substituted into another replacement.
substAll :: [(Name, Expr)] -> Expr -> Expr
substAll bs e0 =
  let placeholders = [ (x, "#" ++ show i) | ((x, _), i) <- zip bs [0 :: Int ..] ]
      renamed = foldl (\e (x, p) -> rename x p e) e0 placeholders
  in  foldl (\e ((_, n), (_, p)) -> subst p n e) renamed (zip bs placeholders)

-- | A name not in the taken set, preferring primes: @x@, @x'@, @x''@, ...
fresh :: [Name] -> Name -> Name
fresh taken x
  | x `notElem` taken = x
  | otherwise         = fresh taken (x ++ "'")

hasHole :: Expr -> Bool
hasHole (Hole _)  = True
hasHole (Var _)   = False
hasHole (Lit _)   = False
hasHole (Lam _ _ e) = hasHole e
hasHole (App f a) = hasHole f || hasHole a
hasHole (Subst bs m) = any (hasHole . snd) bs || hasHole m

hasSubst :: Expr -> Bool
hasSubst Subst {} = True
hasSubst (Lam _ _ e) = hasSubst e
hasSubst (App f a) = hasSubst f || hasSubst a
hasSubst _ = False

-- | Equality up to renaming of bound variables. Free names are
-- compared literally; annotations are ignored. Holes are never equal.
alphaEq :: Expr -> Expr -> Bool
alphaEq = go [] []
  where
    go e1 e2 (Var x) (Var y) = resolve e1 x == resolve e2 y
    go e1 e2 (Lam x _ a) (Lam y _ b) =
      let n = length e1
      in  go ((x, n) : e1) ((y, n) : e2) a b
    go e1 e2 (App a b) (App c d) = go e1 e2 a c && go e1 e2 b d
    go _ _ (Lit a) (Lit b) = a == b
    go e1 e2 (Subst bs m) (Subst bs' m') =
      length bs == length bs'
      && and (zipWith (\(_, n) (_, n') -> go e1 e2 n n') bs bs')
      && go e1 e2 (foldr (\(x, _) b -> Lam x Nothing b) m bs)
                  (foldr (\(y, _) b -> Lam y Nothing b) m' bs')
    go _ _ _ _ = False

    resolve env x = case lookup x env of
      Just i  -> Right i
      Nothing -> Left x

-- | All binder names, in order of appearance (with repetitions).
binders :: Expr -> [Name]
binders (Lam x _ e) = x : binders e
binders (App f a) = binders f ++ binders a
binders (Subst bs m) = concatMap (binders . snd) bs ++ binders m
binders _ = []

-- | Barendregt convention: binders pairwise distinct and distinct from
-- the free names.
barendregt :: Expr -> Bool
barendregt e =
  let bs = binders e
  in  length bs == length (nub bs) && all (`notElem` freeVars e) bs

-- | Drop Church annotations.
erase :: Expr -> Expr
erase (Lam x _ e) = Lam x Nothing (erase e)
erase (App f a) = App (erase f) (erase a)
erase (Subst bs m) = Subst [ (x, erase n) | (x, n) <- bs ] (erase m)
erase e = e

-- | Names from the environment mentioned in a term (free occurrences).
referencedNames :: [(Name, Expr)] -> Expr -> [Name]
referencedNames env e = filter (`elem` map fst env) (freeVars e)

-- | Names a term depends on, transitively through the environment.
namesUsedBy :: [(Name, Expr)] -> Expr -> [Name]
namesUsedBy env = go []
  where
    go seen e =
      let direct = filter (`notElem` seen) (referencedNames env e)
          seen'  = seen ++ direct
      in  foldl (\acc n -> maybe acc (go acc) (lookup n env)) seen' direct

-- | Unfold every environment name, recursively. The environment is
-- acyclic (the loader forbids recursion), so this terminates.
expandAll :: [(Name, Expr)] -> Expr -> Expr
expandAll env = go []
  where
    go bound (Var x)
      | x `elem` bound = Var x
      | Just def <- lookup x env = go [] def
      | otherwise = Var x
    go bound (Lam x t e) = Lam x t (go (x : bound) e)
    go bound (App f a) = App (go bound f) (go bound a)
    go bound (Subst bs m) = Subst [ (x, go bound n) | (x, n) <- bs ] (go (map fst bs ++ bound) m)
    go _ e = e

-- | Full η-reduction: @\\x. f x@ becomes @f@ when @x@ is not free in @f@.
etaReduce :: Expr -> Expr
etaReduce e = case e of
  Lam x t body ->
    case etaReduce body of
      App f (Var y) | y == x, x `notElem` freeVars f -> f
      body' -> Lam x t body'
  App f a -> App (etaReduce f) (etaReduce a)
  _ -> e

-- | Every term obtained by one η-contraction (@\\x. f x@ to @f@, where @x@
-- is not free in @f@) somewhere inside the term.
etaContractions :: Expr -> [Expr]
etaContractions e = here ++ inside
  where
    here = case e of
      Lam x _ (App f (Var y)) | y == x, x `notElem` freeVars f -> [f]
      _ -> []
    inside = case e of
      Lam x t body -> [ Lam x t body' | body' <- etaContractions body ]
      App f a -> [ App f' a | f' <- etaContractions f ] ++ [ App f a' | a' <- etaContractions a ]
      _ -> []

parenCount :: String -> Int
parenCount = length . filter (== '(')
