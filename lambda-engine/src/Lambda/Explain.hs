-- | @:explain@: show a term’s structure without reducing it.
--
-- Nested lambdas stay nested in the tree (one binder per @Lam@). The
-- linear line uses the same layout as the pretty-printer. Bound names
-- share a style with their binder, at most five binders; more than
-- that, or a colorless palette, means no highlighting. Free names are
-- left in the default color.
module Lambda.Explain
  ( ExplainView (..)
  , TreePiece (..)
  , maxBinderStyles
  , explain
  , renderExplain
  ) where

import Data.List (intercalate)

import Lambda.Color (Palette (..), paint, withCode)
import Lambda.Pretty (prettyExpr)
import Lambda.Subst (freeVars, hasHole)
import Lambda.Syntax

maxBinderStyles :: Int
maxBinderStyles = 5

-- | A tree line: either a constructor (@, @...@) or a name after a prefix.
data TreePiece
  = TreePlain String
  | TreeNamed String Name (Maybe Int)
  deriving (Eq, Show)

data ExplainView = ExplainView
  { evPretty  :: String
  , evSpans   :: [(Int, Int, Int)]  -- start, length, binder style
  , evFree    :: [(Name, Bool)]     -- True if the name is in the env
  , evHole    :: Bool
  , evDef     :: Maybe (Name, Expr)
  , evTree    :: [TreePiece]
  , evBinders :: Int
  } deriving (Eq, Show)

type BinderEnv = [(Name, Int)]

explain :: [(Name, Expr)] -> Expr -> ExplainView
explain env expr =
  let (pretty, spans, _) = prettyExpl [] 0 0 expr
      (_, pieces)        = subtree [] 0 expr
      frees              = [ (n, any ((== n) . fst) env) | n <- freeVars expr ]
      def                = case expr of
        Var n | Just rhs <- lookup n env -> Just (n, rhs)
        _ -> Nothing
  in  ExplainView
        { evPretty  = pretty
        , evSpans   = spans
        , evFree    = frees
        , evHole    = hasHole expr
        , evDef     = def
        , evTree    = pieces
        , evBinders = binderCount expr
        }

binderCount :: Expr -> Int
binderCount (Lam _ _ e) = 1 + binderCount e
binderCount (App f a) = binderCount f + binderCount a
binderCount _         = 0

------------------------------------------------------------------------
-- Linear pretty-print with binder spans (same layout as 'prettyExpr')
------------------------------------------------------------------------

prettyExpl :: BinderEnv -> Int -> Int -> Expr -> (String, [(Int, Int, Int)], Int)
prettyExpl env next _ (Var x) =
  (x, maybe [] (\s -> [(0, length x, s)]) (lookup x env), next)
prettyExpl _ next _ (Lit n) = (show n, [], next)
prettyExpl _ next _ (Hole _) = ("...", [], next)
prettyExpl _ next _ e@(Subst {}) = (prettyExpr e, [], next)
prettyExpl env next prec (App f a) =
  let (fs, spf, n1) = prettyExpl env next 1 f
      (as, spa, n2) = prettyExpl env n1 2 a
      core  = fs ++ " " ++ as
      spans = spf ++ shiftSpans (length fs + 1) spa
  in  wrap 1 prec core spans n2
prettyExpl env next prec abs'@(Lam {}) =
  let (bound, body, env', n1) = collectLams env next abs'
      names  = map fst bound
      prefix = "\\" ++ unwords names ++ ". "
      (bodyS, spb, n2) = prettyExpl env' n1 0 body
      core  = prefix ++ bodyS
      spans = nameSpans bound ++ shiftSpans (length prefix) spb
  in  wrap 0 prec core spans n2

collectLams :: BinderEnv -> Int -> Expr -> ([(Name, Int)], Expr, BinderEnv, Int)
collectLams env next (Lam x _ e) =
  let env' = (x, next) : env
      (bs, body, env'', n') = collectLams env' (next + 1) e
  in  ((x, next) : bs, body, env'', n')
collectLams env next e = ([], e, env, next)

nameSpans :: [(Name, Int)] -> [(Int, Int, Int)]
nameSpans = go 1
  where
    go _ [] = []
    go i ((name, sty) : rest) =
      (i, length name, sty) : go (i + length name + 1) rest

wrap :: Int -> Int -> String -> [(Int, Int, Int)] -> Int
    -> (String, [(Int, Int, Int)], Int)
wrap exprPrec ctx s spans next
  | exprPrec < ctx = ("(" ++ s ++ ")", shiftSpans 1 spans, next)
  | otherwise      = (s, spans, next)

shiftSpans :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
shiftSpans k = map (\(i, n, s) -> (i + k, n, s))

------------------------------------------------------------------------
-- ASCII tree, one 'Lam' per node
------------------------------------------------------------------------

subtree :: BinderEnv -> Int -> Expr -> (Int, [TreePiece])
subtree env next (Var x) =
  (next, [TreeNamed "" x (lookup x env)])
subtree _ next (Lit n) =
  (next, [TreePlain (show n)])
subtree _ next (Hole _) =
  (next, [TreePlain "..."])
subtree _ next e@(Subst {}) =
  (next, [TreePlain (prettyExpr e)])
subtree env next (Lam x _ e) =
  let env' = (x, next) : env
      (n1, rest) = child env' (next + 1) True e
  in  (n1, TreeNamed "\\ " x (Just next) : rest)
subtree env next (App f a) =
  let (n1, fs) = child env next False f
      (n2, as) = child env n1 True a
  in  (n2, TreePlain "@" : fs ++ as)

child :: BinderEnv -> Int -> Bool -> Expr -> (Int, [TreePiece])
child env next isLast e =
  let (n, pcs) = subtree env next e
      branch = if isLast then "`-- " else "|-- "
      hang   = if isLast then "    " else "|   "
  in  (n, indentPiece branch hang pcs)

indentPiece :: String -> String -> [TreePiece] -> [TreePiece]
indentPiece _ _ [] = []
indentPiece branch hang (p : ps) =
  prefixPiece branch p : map (prefixPiece hang) ps

prefixPiece :: String -> TreePiece -> TreePiece
prefixPiece pre (TreePlain s)     = TreePlain (pre ++ s)
prefixPiece pre (TreeNamed p n s) = TreeNamed (pre ++ p) n s

------------------------------------------------------------------------
-- Render
------------------------------------------------------------------------

renderExplain :: Palette -> ExplainView -> String
renderExplain pal view =
  intercalate "\n" $
    [ paintedPretty pal view
    , ""
    , freeLine pal view
    ]
    ++ [ holeLine pal | evHole view ]
    ++ [ defLine name rhs | Just (name, rhs) <- [evDef view] ]
    ++ [ "" ]
    ++ map (renderPiece pal view) (evTree view)

paintedPretty :: Palette -> ExplainView -> String
paintedPretty pal view
  | highlight pal view =
      foldr paintOne (evPretty view) (evSpans view)
  | otherwise = evPretty view
  where
    paintOne (i, n, k) =
      paint (binderCode pal k) (palReset pal) (i, n)

highlight :: Palette -> ExplainView -> Bool
highlight pal view =
  evBinders view > 0
  && evBinders view <= maxBinderStyles
  && not (all null (palBinders pal))

binderCode :: Palette -> Int -> String
binderCode pal k =
  case drop k (palBinders pal) of
    (c : _) -> c
    []      -> ""

freeLine :: Palette -> ExplainView -> String
freeLine pal view =
  let lab = withCode (palMuted pal) (palReset pal) "free"
      body = case evFree view of
        [] -> withCode (palMuted pal) (palReset pal) "(none)"
        xs -> intercalate ", " (map (freeItem pal) xs)
  in  lab ++ "  " ++ body

freeItem :: Palette -> (Name, Bool) -> String
freeItem _ (name, False) = name
freeItem pal (name, True) =
  name ++ " " ++ withCode (palMuted pal) (palReset pal) "(env)"

holeLine :: Palette -> String
holeLine pal =
  withCode (palMuted pal) (palReset pal) "hole" ++ "  ..."

defLine :: Name -> Expr -> String
defLine name rhs = name ++ " := " ++ prettyExpr rhs

renderPiece :: Palette -> ExplainView -> TreePiece -> String
renderPiece _ _ (TreePlain s) = s
renderPiece pal view (TreeNamed pre name sty) =
  pre ++ paintName pal view name sty

paintName :: Palette -> ExplainView -> Name -> Maybe Int -> String
paintName pal view name (Just k)
  | highlight pal view =
      withCode (binderCode pal k) (palReset pal) name
paintName _ _ name _ = name
