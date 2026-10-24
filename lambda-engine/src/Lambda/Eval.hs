-- | Reduction: find a redex, contract it, reduce to normal form, compare.
--
-- Redexes:
--
-- * β — @(\\x. e) a@
-- * δ — a free name bound in the environment
-- * π — a primitive applied to literals (@language typed@ only)
--
-- Strategies:
--
-- * 'Lazy' (normal order) — leftmost-outermost.
-- * 'Strict' — leftmost call-by-value (arguments to a value first).
-- * 'Applicative' — leftmost-innermost (all arguments and bodies first).
--
-- All strategies reduce under lambdas when asked for a full normal form.
-- Definitions are unfolded only when the name is the next redex; bound
-- variables never δ-reduce.
module Lambda.Eval
  ( Ctx (..)
  , pureCtx
  , Strategy (..)
  , Path (..)
  , EvalError (..)
  , RedexKind (..)
  , Step (..)
  , defaultLimit
  , parseStrategy
  , prettyStrategy
  , findRedex
  , contract
  , stepBeta
  , followSteps
  , normalForm
  , normalFormWith
  , normalize
    -- * Enumerating single steps
  , betaPaths
  , contractBeta
  , unfoldings
  , namedUnfoldings
  , unfoldingsOf
  , reachable
    -- * Comparison and shapes
  , Verdict (..)
  , betaEq
  , isWhnf
  , isHnf
  , isNf
  , normalizing
  , primNames
  , spine
  ) where

import Data.Maybe (isJust)

import Lambda.Need (normalizeNeed)
import Lambda.Subst (alphaEq, etaReduce, expandAll, freeVars, fresh, subst)
import Lambda.Syntax

-- | Evaluation context: definitions plus whether primitives are on.
data Ctx = Ctx
  { ctxEnv   :: [(Name, Expr)]
  , ctxTyped :: Bool
  } deriving (Eq, Show)

pureCtx :: [(Name, Expr)] -> Ctx
pureCtx env = Ctx env False

data Strategy
  = Lazy
  | Strict
  | Applicative
  deriving (Eq, Show)

-- | Location of a subterm: the next redex, or a pretty-printer focus.
data Path
  = Here
  | InFun Path
  | InArg Path
  | InBody Path
  deriving (Eq, Show)

data EvalError
  = TooManySteps
  deriving (Eq, Show)

-- | What kind of redex a step contracts; δ and π carry the name that
-- was unfolded or computed, so traces can print @~K~>@ / @~plus~>@.
data RedexKind
  = Beta
  | Delta Name
  | Prim Name
  deriving (Eq, Show)

data Step
  = NoRedex Expr
  | Stepped RedexKind Path Expr Expr  -- kind, path, before, after
  deriving (Eq, Show)

defaultLimit :: Int
defaultLimit = 10000

parseStrategy :: String -> Maybe Strategy
parseStrategy "lazy"        = Just Lazy
parseStrategy "normal"      = Just Lazy
parseStrategy "strict"      = Just Strict
parseStrategy "applicative" = Just Applicative
parseStrategy _             = Nothing

prettyStrategy :: Strategy -> String
prettyStrategy Lazy        = "normal"
prettyStrategy Strict      = "strict"
prettyStrategy Applicative = "applicative"

------------------------------------------------------------------------
-- Primitives (typed language)
------------------------------------------------------------------------

primNames :: [Name]
primNames = ["true", "false", "plus", "minus", "mult", "eq", "lt", "iszero", "if"]

-- | One primitive step at the root, if the term is a saturated primitive
-- application with literal arguments.
primStep :: Expr -> Maybe Expr
primStep e = case spine e of
  (Var "plus",   [Lit a, Lit b]) -> Just (Lit (a + b))
  (Var "minus",  [Lit a, Lit b]) -> Just (Lit (a - b))
  (Var "mult",   [Lit a, Lit b]) -> Just (Lit (a * b))
  (Var "eq",     [Lit a, Lit b]) -> Just (bool (a == b))
  (Var "lt",     [Lit a, Lit b]) -> Just (bool (a < b))
  (Var "iszero", [Lit a])        -> Just (bool (a == 0))
  (Var "if",     [Var "true", a, _])  -> Just a
  (Var "if",     [Var "false", _, b]) -> Just b
  _ -> Nothing
  where
    bool True  = Var "true"
    bool False = Var "false"

spine :: Expr -> (Expr, [Expr])
spine = go []
  where
    go args (App f a) = go (a : args) f
    go args e = (e, args)

isPrimRedex :: Ctx -> [Name] -> Expr -> Bool
isPrimRedex ctx bound e =
  ctxTyped ctx && case fst (spine e) of
    Var p | p `elem` primNames, p `notElem` bound -> isJust (primStep e)
    _ -> False

------------------------------------------------------------------------
-- Redex search
------------------------------------------------------------------------

-- | Values for call-by-value: lambdas, literals, and stuck variables
-- (so @(\\x. x) y@ is still a redex). A name from the environment is
-- not a value: it has to be unfolded first, so @strict@ really does
-- evaluate an argument such as @omega@ before the call.
isValue :: Ctx -> [Name] -> Expr -> Bool
isValue ctx bound e = case e of
  Lam{} -> True
  Lit{} -> True
  Var x -> not (inEnv ctx bound x)
  _     -> False

isBetaRedex :: Expr -> Bool
isBetaRedex (App Lam{} _) = True
isBetaRedex _             = False

inEnv :: Ctx -> [Name] -> Name -> Bool
inEnv ctx bound x = x `notElem` bound && any ((== x) . fst) (ctxEnv ctx)

-- | Next redex according to the strategy, or 'Nothing' if the term
-- is already in normal form (holes and unknown variables are stuck).
findRedex :: Strategy -> Ctx -> Expr -> Maybe Path
findRedex strat ctx = go []
  where
    go bound (Var x)
      | inEnv ctx bound x = Just Here
      | otherwise         = Nothing
    go _ (Hole _) = Nothing
    go _ (Lit _) = Nothing
    go _ Subst {} = Nothing
    go bound (Lam x _ body) = fmap InBody (go (x : bound) body)
    go bound app@(App f a) = case strat of
      Lazy
        | isBetaRedex app || isPrimRedex ctx bound app -> Just Here
        | otherwise -> fmap InFun (go bound f) `orElse` fmap InArg (go bound a)
      Strict
        | isBetaRedex app && value a -> Just Here
        | isPrimRedex ctx bound app  -> Just Here
        | not (value f)              -> fmap InFun (go bound f)
        | not (value a)              -> fmap InArg (go bound a)
        | otherwise -> Nothing
        where value = isValue ctx bound
      Applicative ->
        fmap InFun (go bound f) `orElse` fmap InArg (go bound a) `orElse`
          (if isBetaRedex app || isPrimRedex ctx bound app then Just Here else Nothing)

    orElse :: Maybe a -> Maybe a -> Maybe a
    orElse (Just x) _ = Just x
    orElse Nothing  y = y

focused :: Path -> Expr -> Maybe Expr
focused Here e = Just e
focused (InFun p) (App f _) = focused p f
focused (InArg p) (App _ a) = focused p a
focused (InBody p) (Lam _ _ e) = focused p e
focused _ _ = Nothing

-- | Contract the redex at the given path. If the path is stale, the
-- term is returned unchanged. δ copies the current environment RHS
-- (late binding). Binders on the path are renamed when they would
-- capture free names of that RHS.
contract :: Ctx -> Path -> Expr -> Expr
contract ctx path e =
  case focused path e of
    Just (Var x) | Just def <- lookup x (ctxEnv ctx) -> insertAt path def e
    Just sub | ctxTyped ctx, Just r <- primStep sub -> insertAt path r e
    _ -> contractBeta path e

contractBeta :: Path -> Expr -> Expr
contractBeta Here (App (Lam x _ body) arg) = subst x arg body
contractBeta (InFun p)  (App f a) = App (contractBeta p f) a
contractBeta (InArg p)  (App f a) = App f (contractBeta p a)
contractBeta (InBody p) (Lam x t e) = Lam x t (contractBeta p e)
contractBeta _ e = e

-- | Replace the subterm at the path, renaming binders that would
-- capture free names of the inserted term.
insertAt :: Path -> Expr -> Expr -> Expr
insertAt Here s _ = s
insertAt (InFun p) s (App f a) = App (insertAt p s f) a
insertAt (InArg p) s (App f a) = App f (insertAt p s a)
insertAt (InBody p) s (Lam x t e)
  | x `elem` freeVars s =
      let x' = fresh (x : freeVars s ++ freeVars e) x
      in  Lam x' t (insertAt p s (subst x (Var x') e))
  | otherwise = Lam x t (insertAt p s e)
insertAt _ _ e = e

redexKind :: Ctx -> Path -> Expr -> RedexKind
redexKind ctx path e =
  case focused path e of
    Just (Var x) | any ((== x) . fst) (ctxEnv ctx) -> Delta x
    Just sub | ctxTyped ctx, Just _ <- primStep sub, (Var p, _) <- spine sub -> Prim p
    _ -> Beta

-- | One step, whichever is next for the strategy.
stepBeta :: Strategy -> Ctx -> Expr -> Step
stepBeta strat ctx e =
  case findRedex strat ctx e of
    Nothing -> NoRedex e
    Just p  -> Stepped (redexKind ctx p e) p e (contract ctx p e)

-- | Collect every step until normal form (including a final
-- 'NoRedex'). Used by @:follow@.
followSteps :: Strategy -> Ctx -> Int -> Expr -> Either EvalError [Step]
followSteps strat ctx = go
  where
    go n e = case stepBeta strat ctx e of
      done@(NoRedex _) -> Right [done]
      step@(Stepped _ _ _ after)
        | n <= 0    -> Left TooManySteps
        | otherwise -> fmap (step :) (go (n - 1) after)

-- | Normal-order full normal form with an empty environment.
normalForm :: Int -> Expr -> Either EvalError Expr
normalForm = normalFormWith Lazy (pureCtx [])

normalFormWith :: Strategy -> Ctx -> Int -> Expr -> Either EvalError Expr
normalFormWith strat ctx = go
  where
    go n e =
      case stepBeta strat ctx e of
        NoRedex e' -> Right e'
        Stepped _ _ _ e'
          | n <= 0    -> Left TooManySteps
          | otherwise -> go (n - 1) e'

-- | Fuel of the call-by-need normaliser: β-steps plus nodes of the result.
-- Generous, because shared steps are cheap; a diverging term burns it in a
-- fraction of a second.
needFuel :: Int
needFuel = 500000

-- | Normal form when only the result matters (@expect@, @:nf@, @:decode@),
-- not the steps. Pure terms go through "Lambda.Need", which evaluates every
-- argument at most once; without it a solution that uses an argument several
-- times needs exponentially many steps. If that gives up, or primitives are
-- on, fall back to step-by-step normal order within @limit@ steps.
normalize :: Ctx -> Int -> Expr -> Either EvalError Expr
normalize ctx limit e
  | not (ctxTyped ctx), Just nf <- normalizeNeed (ctxEnv ctx) needFuel e = Right nf
  | otherwise = normalFormWith Lazy ctx limit e

------------------------------------------------------------------------
-- Enumerating single steps (for chains)
------------------------------------------------------------------------

-- | Paths of every β-redex (names stay opaque), leftmost-outermost first.
betaPaths :: Expr -> [Path]
betaPaths e = case e of
  App f a ->
    [Here | isBetaRedex e] ++ map InFun (betaPaths f) ++ map InArg (betaPaths a)
  Lam _ _ b -> map InBody (betaPaths b)
  _ -> []

-- | Every term obtained by unfolding one free occurrence of one
-- environment name, tagged with that name.
namedUnfoldings :: Ctx -> Expr -> [(Name, Expr)]
namedUnfoldings ctx = go []
  where
    go bound (Var x)
      | inEnv ctx bound x, Just def <- lookup x (ctxEnv ctx) = [(x, def)]
      | otherwise = []
    go bound (Lam x t b) = [(n, Lam x t b') | (n, b') <- go (x : bound) b]
    go bound (App f a) = [(n, App f' a) | (n, f') <- go bound f] ++ [(n, App f a') | (n, a') <- go bound a]
    go _ _ = []

unfoldings :: Ctx -> Expr -> [Expr]
unfoldings ctx = map snd . namedUnfoldings ctx

-- | Unfoldings of one particular name (@~K~>@ in a chain).
unfoldingsOf :: Ctx -> Name -> Expr -> [Expr]
unfoldingsOf ctx name = map snd . filter ((== name) . fst) . namedUnfoldings ctx

-- | Terms reachable in at most @k@ β/δ steps (any redex), α-deduplicated.
-- The frontier is capped so pathological terms do not explode.
reachable :: Ctx -> Int -> Expr -> [Expr]
reachable ctx k0 e0 = go k0 [e0] [e0]
  where
    cap = 2000
    go 0 _ seen = seen
    go k frontier seen =
      let next = [ e' | e <- frontier, e' <- successors e ]
          fresh' = dedupe seen next
          seen' = seen ++ fresh'
      in  if null fresh' || length seen' > cap then seen' else go (k - 1 :: Int) fresh' seen'
    successors e =
      [ contractBeta p e | p <- betaPaths e ] ++ unfoldings ctx e
    dedupe seen = foldl (\acc e -> if any (alphaEq e) (seen ++ acc) then acc else acc ++ [e]) []

------------------------------------------------------------------------
-- Shapes
------------------------------------------------------------------------

-- | Weak head normal form: a lambda, a literal, or a stuck application
-- (no redex at the head). Names are opaque here; expand first if needed.
isWhnf :: Expr -> Bool
isWhnf e = case e of
  Lam {} -> True
  Lit _ -> True
  _ -> case spine e of
    (Var _, _) -> True
    _ -> False

-- | Head normal form: @\\x1 .. xn. y a1 .. am@.
isHnf :: Expr -> Bool
isHnf (Lam _ _ b) = isHnf b
isHnf e = case spine e of
  (Var _, _) -> True
  (Lit _, []) -> True
  _ -> False

-- | β-normal form: no β-redex anywhere.
isNf :: Expr -> Bool
isNf = null . betaPaths

-- | Approximation of strong normalisation: the term reaches a normal
-- form both in normal order and in applicative order within the limit.
normalizing :: Ctx -> Int -> Expr -> Bool
normalizing ctx limit e =
  either (const False) (const True) (normalFormWith Lazy ctx limit e)
  && either (const False) (const True) (normalFormWith Applicative ctx limit e)

------------------------------------------------------------------------
-- β-equivalence
------------------------------------------------------------------------

data Verdict
  = Equal
  | Differ Expr Expr        -- ^ normal forms (or head normal forms) that differ
  | Undecided               -- ^ ran out of steps
  deriving (Eq, Show)

-- | Decide βη-equivalence as far as a step budget allows. Both sides are
-- fully δ-expanded first. Fast path: both normalise, and the normal
-- forms are compared after η-reduction (so @pow 5 0@, which is @I@, is
-- accepted as ⌜1⌝). Otherwise the terms are compared head-first (a
-- bounded Böhm-tree comparison), so @Y g m =β m (Y g)@ is decidable
-- even though @Y g@ has no normal form.
betaEq :: Ctx -> Int -> Expr -> Expr -> Verdict
betaEq ctx limit a0 b0 =
  let a = expandAll (ctxEnv ctx) a0
      b = expandAll (ctxEnv ctx) b0
      ctx' = ctx { ctxEnv = [] }
  in  if alphaEq a b then Equal else
      case (normalize ctx' limit a, normalize ctx' limit b) of
        (Right na, Right nb)
          | alphaEq (etaReduce na) (etaReduce nb) -> Equal
          | otherwise -> Differ na nb
        _ -> case bohm ctx' limit a b of
          Undecided
            | commonReduct ctx' 5 a b -> Equal
            | otherwise -> Undecided
          v -> v

-- | Do the two terms have a common reduct within @k@ β-steps each?
-- The last resort for terms without a head normal form, such as the
-- unsolvable solutions of @F x y = F y (x F)@.
commonReduct :: Ctx -> Int -> Expr -> Expr -> Bool
commonReduct ctx k a b =
  let ra = reachable ctx k a
      rb = reachable ctx k b
  in  or [ alphaEq x y | x <- ra, y <- rb ]

bohm :: Ctx -> Int -> Expr -> Expr -> Verdict
bohm ctx fuel0 a0 b0 = case go fuel0 [] [] [] a0 b0 of
  Left v -> v
  Right _ -> Equal
  where
    -- e1/e2 map binder names to levels for α-comparison of heads;
    -- seen holds pairs already under comparison on this path: meeting
    -- one again means the Böhm trees are regular and equal so far
    -- (coinduction), which is what makes Y-based definitions decidable.
    go fuel seen e1 e2 a b
      | fuel <= 0 = Left Undecided
      | alphaEq a b = Right fuel
      | any (\(x, y) -> alphaEq a x && alphaEq b y) seen = Right fuel
      | otherwise =
          case (headNF ctx fuel a, headNF ctx fuel b) of
            (Nothing, _) -> Left Undecided
            (_, Nothing) -> Left Undecided
            (Just (a', fa), Just (b', fb)) ->
              let fuel' = min fa fb
                  (xs, ha, argsA) = peel a'
                  (ys, hb, argsB) = peel b'
              in  if length xs /= length ys || length argsA /= length argsB
                    then Left (Differ a' b')
                    else
                      let n = length e1
                          e1' = zip xs [n ..] ++ e1
                          e2' = zip ys [n ..] ++ e2
                      in  if not (sameHead e1' e2' ha hb)
                            then Left (Differ a' b')
                            else foldArgs fuel' ((a, b) : seen) e1' e2' (zip argsA argsB)

    foldArgs fuel _ _ _ [] = Right fuel
    foldArgs fuel seen e1 e2 ((x, y) : rest) =
      case go fuel seen e1 e2 x y of
        Left v -> Left v
        Right fuel' -> foldArgs fuel' seen e1 e2 rest

    sameHead e1 e2 (Var x) (Var y) = resolve e1 x == resolve e2 y
    sameHead _ _ (Lit x) (Lit y) = x == y
    sameHead _ _ _ _ = False

    resolve env x = case lookup x env of
      Just i  -> Right i
      Nothing -> Left x

    peel (Lam x _ b) = let (xs, h, as) = peel b in (x : xs, h, as)
    peel e = let (h, as) = spine e in ([], h, as)

-- | Reduce to head normal form (contract head redexes only), returning
-- the remaining fuel. 'Nothing' if the fuel runs out.
headNF :: Ctx -> Int -> Expr -> Maybe (Expr, Int)
headNF ctx = go
  where
    go fuel e
      | fuel <= 0 = Nothing
      | otherwise = case e of
          Lam x t b -> case go fuel b of
            Just (b', f) -> Just (Lam x t b', f)
            Nothing -> Nothing
          _ -> case headRedex e of
            Nothing -> Just (e, fuel)
            Just e' -> go (fuel - 1) e'

    headRedex e = case spine e of
      (Lam x _ body, a : rest) -> Just (foldl App (subst x a body) rest)
      _ | ctxTyped ctx, Just r <- primStep e -> Just r
        | otherwise -> Nothing
