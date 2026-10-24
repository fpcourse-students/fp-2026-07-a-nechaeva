-- | Print terms and types in the concrete syntax the parser accepts.
module Lambda.Pretty
  ( prettyExpr
  , prettyFocus
  , prettyCaret
  , prettyType
  , prettyTypeGreek
  , prettyTypeGreek2
  ) where

import Data.List (nub)
import Data.Maybe (fromMaybe)

import Lambda.Eval (Path (..))
import Lambda.Syntax

-- | Pretty-print a term. Nested lambdas are collected: @\\x. \\y. e@
-- becomes @\\x y. e@.
prettyExpr :: Expr -> String
prettyExpr = fst . prettyAnn Nothing 0

-- | Pretty-print with a focused subterm. The span is the start index
-- and length of that subterm in the resulting string (including any
-- parentheses the focus itself needs).
prettyFocus :: Path -> Expr -> (String, (Int, Int))
prettyFocus path expr =
  case prettyAnn (Just path) 0 expr of
    (s, Just sp) -> (s, sp)
    (s, Nothing) -> (s, (0, length s))

-- | A second line with @^~~~~@ under a span.
prettyCaret :: (Int, Int) -> String
prettyCaret (i, n) =
  replicate (max 0 i) ' '
    ++ "^"
    ++ replicate (max 0 (n - 1)) '~'

-- Precedence: lambda = 0, application = 1, atom = 2.
prettyAnn :: Maybe Path -> Int -> Expr -> (String, Maybe (Int, Int))
prettyAnn (Just Here) prec expr =
  let (s, _) = prettyAnn Nothing prec expr
  in  (s, Just (0, length s))
-- Precedences: 0 lambda, 1 comparison (< ==), 2 additive (+ -), 3 multiplicative (*),
-- 4 application, atoms never wrapped — the same levels the parser uses.
prettyAnn focus prec expr = case expr of
  Var x  -> (x, Nothing)
  Lit n  -> (show n, Nothing)
  Hole _ -> ("...", Nothing)
  Subst bs m ->
    let binds = [ x ++ " |-> " ++ fst (prettyAnn Nothing 0 n) | (x, n) <- bs ]
        (ms, _) = prettyAnn Nothing 4 m
    in  applyWrap 4 prec ("[" ++ joinWith ", " binds ++ "] " ++ ms) Nothing
  -- a saturated primitive with an operator spelling prints infix: @plus x 1@ is @x + 1@
  App (App (Var op) a) b | Just (sym, p) <- lookup op infixOps ->
    let aFocus = case focus of
          Just (InFun (InArg q)) -> Just q
          _                      -> Nothing
        bFocus = case focus of
          Just (InArg q) -> Just q
          _              -> Nothing
        opFocus = case focus of
          Just (InFun (InFun _)) -> True
          _                      -> False
        (as, spa) = prettyAnn aFocus p a
        (bs, spb) = prettyAnn bFocus (p + 1) b
        core = as ++ " " ++ sym ++ " " ++ bs
        sp | opFocus   = Just (length as + 1, length sym)
           | otherwise = case spa of
               Just s  -> Just s
               Nothing -> shiftSpan (length as + length sym + 2) spb
    in  applyWrap p prec core sp
  App f a ->
    let funFocus = case focus of
          Just (InFun p) -> Just p
          _              -> Nothing
        argFocus = case focus of
          Just (InArg p) -> Just p
          _              -> Nothing
        (fs, spf) = prettyAnn funFocus 4 f
        (as, spa) = prettyAnn argFocus 5 a
        core = fs ++ " " ++ as
        sp = case spf of
          Just s  -> Just s
          Nothing -> shiftSpan (length fs + 1) spa
    in  applyWrap 4 prec core sp
  abs'@(Lam {}) ->
    let (xs, body, bodyFocus) = splitLams focus abs'
        (bodyS, spb) = prettyAnn bodyFocus 0 body
        prefix = "\\" ++ unwords (map binderText xs) ++ ". "
        core = prefix ++ bodyS
        sp = shiftSpan (length prefix) spb
    in  applyWrap 0 prec core sp

-- | Primitive names with an infix spelling and its precedence (the parser
-- reads the operators, the printer writes them back).
infixOps :: [(Name, (String, Int))]
infixOps =
  [ ("lt", ("<", 1)), ("eq", ("==", 1))
  , ("plus", ("+", 2)), ("minus", ("-", 2))
  , ("mult", ("*", 3)) ]

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith sep (x : xs) = x ++ concatMap (sep ++) xs

binderText :: (Name, Maybe Type) -> String
binderText (x, Nothing) = x
binderText (x, Just t) = x ++ ":" ++ prettyTypeAtom t

splitLams :: Maybe Path -> Expr -> ([(Name, Maybe Type)], Expr, Maybe Path)
splitLams (Just (InBody p)) (Lam x t e) =
  let (xs, body, f) = splitLams (Just p) e
  in  ((x, t) : xs, body, f)
splitLams Nothing (Lam x t e) =
  let (xs, body, f) = splitLams Nothing e
  in  ((x, t) : xs, body, f)
splitLams _ (Lam x t e) = ([(x, t)], e, Nothing)
splitLams focus other = ([], other, focus)

applyWrap :: Int -> Int -> String -> Maybe (Int, Int) -> (String, Maybe (Int, Int))
applyWrap exprPrec_ ctx s sp
  | exprPrec_ < ctx = ("(" ++ s ++ ")", shiftSpan 1 sp)
  | otherwise       = (s, sp)

shiftSpan :: Int -> Maybe (Int, Int) -> Maybe (Int, Int)
shiftSpan k (Just (i, n)) = Just (i + k, n)
shiftSpan _ Nothing       = Nothing

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

prettyType :: Type -> String
prettyType (TVar a) = a
prettyType (TCon c) = c
prettyType (TArr a b) = prettyTypeAtom' a ++ " -> " ++ prettyType b
  where
    prettyTypeAtom' t@(TArr _ _) = "(" ++ prettyType t ++ ")"
    prettyTypeAtom' t = prettyType t

prettyTypeAtom :: Type -> String
prettyTypeAtom t@(TArr _ _) = "(" ++ prettyType t ++ ")"
prettyTypeAtom t = prettyType t

-- | Print with generated type variables (names starting with @?@)
-- renamed to α, β, γ, … in order of first appearance. Variables named
-- by the user keep their names.
prettyTypeGreek :: Type -> String
prettyTypeGreek t = prettyType (greekRename (collectVars t) t)

-- | Print two types with one consistent renaming.
prettyTypeGreek2 :: Type -> Type -> (String, String)
prettyTypeGreek2 a b =
  let vars = collectVars (TArr a b)
  in  (prettyType (greekRename vars a), prettyType (greekRename vars b))

collectVars :: Type -> [Name]
collectVars = nub . go
  where
    go (TVar a) = [a]
    go (TCon _) = []
    go (TArr a b) = go a ++ go b

greekRename :: [Name] -> Type -> Type
greekRename vars = rename
  where
    generated = filter ((== "?") . take 1) vars
    taken = filter ((/= "?") . take 1) vars
    greek = filter (`notElem` taken) (map (: []) "αβγδεζηθικ" ++ [ "τ" ++ show i | i <- [1 :: Int ..] ])
    table = zip generated greek
    rename (TVar a) = TVar (fromMaybe a (lookup a table))
    rename (TCon c) = TCon c
    rename (TArr a b) = TArr (rename a) (rename b)
