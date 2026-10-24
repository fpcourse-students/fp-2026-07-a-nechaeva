-- | Recognise standard encodings in normal forms and print them the way
-- the lecture does: ⌜6⌝, true, ⟨a, b⟩, [1, 2]. The object language is
-- untouched; this is only for messages.
module Lambda.Decode
  ( decode
  , prettyDecoded
  , numeral
  ) where

import Data.List (intercalate)

import Lambda.Pretty (prettyExpr)
import Lambda.Subst (freeVars)
import Lambda.Syntax

-- | A term with recognised sub-encodings printed readably. Falls back
-- to the plain pretty-printer.
prettyDecoded :: Expr -> String
prettyDecoded e = case decode e of
  Just s -> s
  Nothing -> case e of
    Lam {} -> prettyExpr e
    App _ _ -> unwords (map atom (spineList e))
    _ -> prettyExpr e
  where
    atom sub = case decode sub of
      Just s -> s
      Nothing -> case sub of
        Var x -> x
        Lit n -> show n
        Hole _ -> "..."
        _ -> "(" ++ prettyDecoded sub ++ ")"
    spineList (App f a) = spineList f ++ [a]
    spineList x = [x]

-- | Decode a whole term, if it is a Church numeral, boolean, list or pair.
decode :: Expr -> Maybe String
decode e = case numeral e of
  Just n -> Just ("⌜" ++ show n ++ "⌝")
  Nothing -> case boolean e of
    Just b -> Just (if b then "true" else "false")
    Nothing -> case list e of
      Just xs -> Just ("[" ++ intercalate ", " (map prettyDecoded xs) ++ "]")
      Nothing -> case pair e of
        Just (a, b) -> Just ("⟨" ++ prettyDecoded a ++ ", " ++ prettyDecoded b ++ "⟩")
        Nothing -> Nothing

-- | @\\s z. s (s (... z))@
numeral :: Expr -> Maybe Integer
numeral (Lam s _ (Lam z _ body)) | s /= z = go 0 body
  where
    go n (Var v) | v == z = Just n
    go n (App (Var f) rest) | f == s = go (n + 1) rest
    go _ _ = Nothing
numeral _ = Nothing

-- | @\\a b. a@ / @\\a b. b@
boolean :: Expr -> Maybe Bool
boolean (Lam a _ (Lam b _ (Var v)))
  | a /= b && v == a = Just True
  | a /= b && v == b = Just False
boolean _ = Nothing

-- | Church list @\\c n. c x1 (c x2 (... n))@ with at least one element
-- (the empty list is printed as ⌜0⌝, which is the same term).
list :: Expr -> Maybe [Expr]
list (Lam c _ (Lam n _ body)) | c /= n = go body
  where
    go (Var v) | v == n = Just []
    go (App (App (Var f) x) rest)
      | f == c, c `notElem` freeVars x, n `notElem` freeVars x = (x :) <$> go rest
    go _ = Nothing
list _ = Nothing

-- | @\\p. p a b@ where @p@ is not used in @a@, @b@.
pair :: Expr -> Maybe (Expr, Expr)
pair (Lam p _ (App (App (Var q) a) b))
  | p == q, p `notElem` freeVars a, p `notElem` freeVars b = Just (a, b)
pair _ = Nothing
