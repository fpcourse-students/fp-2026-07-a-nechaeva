-- | REPL commands, all introduced by @:@ like GHCi (@:nf@, @:eq-a@, @:help@).
module Lambda.Command
  ( ReplCmd (..)
  , CommandResult (..)
  , parseCommand
  , runLine
  , runInput
  , execCommand
  , renderResult
  , replHelp
  ) where

import Control.Applicative (empty, (<|>))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.List (intercalate)
import Control.Monad (when)
import Text.Megaparsec (lookAhead, takeWhile1P, try)

import Lambda.Color (Palette (..), paint, withCode)
import Lambda.Decode (prettyDecoded)
import Lambda.Eval
import Lambda.Explain (ExplainView, explain, renderExplain)
import Lambda.Parser (Parser, ident, pAtom, pExpr, parseWith, sc, symbol, desugarNumerals)
import Lambda.Pretty (prettyCaret, prettyExpr, prettyFocus, prettyTypeGreek, prettyTypeGreek2)
import Lambda.Subst (alphaEq, expandAll, hasHole, namesUsedBy, unboundIn)
import Lambda.Syntax
import Lambda.Types (inferType, prettyTypeError, primTypes)

data ReplCmd
  = CmdNf Strategy Expr
  | CmdEqa Expr Expr
  | CmdStep Strategy Expr
  | CmdFollow Strategy Expr
  | CmdExplain Expr
  | CmdType Expr
  | CmdDecode Expr
  | CmdEnv
  | CmdBind Name Expr
  | CmdReload
  | CmdHelp
  | CmdQuit
  deriving (Eq, Show)

data CommandResult
  = CommandOut String
  | CommandEq Bool
  | CommandStep Step
  | CommandFollow [Step]
  | CommandExplain ExplainView
  | CommandEnv [(Name, Expr)]
  | CommandDef Name Expr
  | CommandErr String
  | CommandReload   -- ^ the driver re-reads the file and replaces the context
  | CommandQuit
  deriving (Eq, Show)

replHelp :: String
replHelp = unlines
  [ "A term by itself is reduced to normal form (normal order):"
  , "  (\\x. x) a"
  , "  mult 2 3"
  , ""
  , "A definition is stored as written (later lines can use the name):"
  , "  K := \\x y. x"
  , "  twice := \\f x. f (f x)"
  , ""
  , "Commands:"
  , "  :nf [STRATEGY] <term>       reduce to normal form"
  , "  :step [STRATEGY] <term>     one step: β, unfolding of a name, or a primitive"
  , "  :follow [STRATEGY] <term>   every step to normal form"
  , "  :explain <term>             structure, binders, free names"
  , "  :type <term>                most general simple type (Curry)"
  , "  :decode <term>              normal form printed as ⌜n⌝, true, ⟨a, b⟩, [..]"
  , "  :eq-a <term> <term>         alpha-equivalence (no reduction)"
  , "  :env                        list bindings in this session"
  , "  :reload, :r                 re-read the file (drops definitions made here)"
  , "  :help                       this message"
  , "  :quit                       leave the REPL"
  , ""
  , "STRATEGY = normal (default; leftmost-outermost), strict (call-by-value),"
  , "           applicative (leftmost-innermost)"
  , "Names from the file are unfolded one at a time, only when they are the next redex."
  , ":follow / :step mark a β-step as ~b~> and the unfolding of K as ~K~>"
  , "For :eq-a, parenthesize compound terms: :eq-a twice (\\f x. f (f x))"
  ]

parseCommand :: String -> Either String ReplCmd
parseCommand = parseWith "<repl>" pCommand

-- | @:@ starts a command; @name = expr@ is a definition; anything
-- else is a term, reduced in normal order to NF.
pCommand :: Parser ReplCmd
pCommand =
      (symbol ":" *> pColonBody)
  <|> pBind
  <|> (CmdNf Lazy <$> pExpr)

-- | @name := term@. Only the look-ahead is backtracked, so a line
-- written with @=@ fails with a hint instead of being read as a term.
pBind :: Parser ReplCmd
pBind = do
  name <- try (ident <* lookAhead (symbol ":=" <|> symbol "="))
  op   <- symbol ":=" <|> symbol "="
  when (op == "=") $
    fail ("a definition is written with ‘:=’: " ++ name ++ " := …")
  CmdBind name <$> pExpr

pColonBody :: Parser ReplCmd
pColonBody = do
  name <- cmdName <|> fail "expected a command after ‘:’ (try :help)"
  case name of
    "nf"      -> CmdNf <$> pStrategy <*> pExpr
    "step"    -> CmdStep <$> pStrategy <*> pExpr
    "step-b"  -> CmdStep <$> pStrategy <*> pExpr
    "follow"  -> CmdFollow <$> pStrategy <*> pExpr
    "explain" -> CmdExplain <$> pExpr
    "type"    -> CmdType <$> pExpr
    "t"       -> CmdType <$> pExpr
    "decode"  -> CmdDecode <$> pExpr
    "eq-a"    -> CmdEqa <$> pCmdTerm <*> pCmdTerm
    "env"     -> return CmdEnv
    "reload"  -> return CmdReload
    "r"       -> return CmdReload
    "help"    -> return CmdHelp
    "h"       -> return CmdHelp
    "quit"    -> return CmdQuit
    "q"       -> return CmdQuit
    _         -> fail ("unknown command ‘:" ++ name ++ "’ (try :help)")

-- | Command word after @:@, allowing hyphens (@step-b@, @eq-a@).
cmdName :: Parser String
cmdName = do
  name <- takeWhile1P (Just "command name") isCmdChar
  sc
  return name
  where
    isCmdChar c = c == '-' || c == '_' || isAsciiLower c || isAsciiUpper c || isDigit c

-- | An optional strategy word; defaults to normal order. Any other word
-- is left to the term parser (it may be a variable named @normal@).
pStrategy :: Parser Strategy
pStrategy = try strategyWord <|> return Lazy
  where
    strategyWord = ident >>= maybe empty return . parseStrategy

-- | Atomic / parenthesized terms, so @:eq-a task1 (\\x. x)@ parses
-- as two arguments rather than an application.
pCmdTerm :: Parser Expr
pCmdTerm = pAtom

runLine :: Ctx -> String -> (Ctx, CommandResult)
runLine ctx line =
  case parseCommand line of
    Left err  -> (ctx, CommandErr (firstParseLine err))
    Right cmd -> execCommand ctx (sugar cmd)
  where
    sugar cmd
      | ctxTyped ctx = cmd
      | otherwise = case cmd of
          CmdNf s e -> CmdNf s (desugarNumerals e)
          CmdEqa a b -> CmdEqa (desugarNumerals a) (desugarNumerals b)
          CmdStep s e -> CmdStep s (desugarNumerals e)
          CmdFollow s e -> CmdFollow s (desugarNumerals e)
          CmdExplain e -> CmdExplain (desugarNumerals e)
          CmdType e -> CmdType (desugarNumerals e)
          CmdDecode e -> CmdDecode (desugarNumerals e)
          CmdBind n e -> CmdBind n (desugarNumerals e)
          other -> other

-- | Like 'runLine', but discards the (possibly updated) context.
runInput :: Ctx -> String -> CommandResult
runInput ctx line = snd (runLine ctx line)

-- | Megaparsec bundles are multi-line (position, source excerpt, caret,
-- then the messages); keep the REPL error to one line of messages.
firstParseLine :: String -> String
firstParseLine err =
  case filter isMsg (lines err) of
    [] -> "error: parse error"
    ms -> "error: " ++ intercalate "; " ms
  where
    isMsg l =
      let t = dropWhile isSpace l
      in  not (null t)
          && take 1 t /= "|"
          && not (isExcerpt t)
          && take 6 t /= "<repl>"
          && not (all (\c -> isSpace c || c == '^') t)
    -- The quoted source line: a line number, a space and a bar.
    isExcerpt t = case span isDigit t of
      (d, ' ' : '|' : _) -> not (null d)
      _ -> False

execCommand :: Ctx -> ReplCmd -> (Ctx, CommandResult)
execCommand ctx CmdHelp =
  (ctx, CommandOut (stripTrailingNewline replHelp))
execCommand ctx CmdQuit = (ctx, CommandQuit)
execCommand ctx CmdReload = (ctx, CommandReload)
execCommand ctx CmdEnv = (ctx, CommandEnv (ctxEnv ctx))
execCommand ctx (CmdFollow strat expr) =
  case resolve ctx expr of
    Left err -> (ctx, CommandErr err)
    Right e  ->
      case followSteps strat ctx defaultLimit e of
        Left TooManySteps ->
          (ctx, CommandErr "error: did not reach normal form (reduction limit)")
        Right steps ->
          case followHole steps of
            Left err    -> (ctx, CommandErr err)
            Right steps' -> (ctx, CommandFollow steps')
execCommand ctx (CmdBind name expr) =
  let env = ctxEnv ctx
      frees = unboundIn (filter ((/= name) . fst) env) expr
      frees' = if ctxTyped ctx then filter (`notElem` primNames) frees else frees
  in  if not (null frees')
        then (ctx, CommandErr (freeMsg name frees'))
        else (ctx { ctxEnv = setBind env name expr }, CommandDef name expr)
execCommand ctx (CmdNf strat expr) =
  case resolve ctx expr of
    Left err -> (ctx, CommandErr err)
    Right e  ->
      case (if strat == Lazy then normalize else normalFormWith strat) ctx defaultLimit e of
        Left TooManySteps ->
          (ctx, CommandErr "error: did not reach normal form (reduction limit)")
        Right nf ->
          case rejectHole nf of
            Left err  -> (ctx, CommandErr err)
            Right nf' -> (ctx, CommandOut (prettyExpr nf'))
execCommand ctx (CmdDecode expr) =
  case resolve ctx expr of
    Left err -> (ctx, CommandErr err)
    Right e  ->
      case normalize ctx defaultLimit e of
        Left TooManySteps ->
          (ctx, CommandErr "error: did not reach normal form (reduction limit)")
        Right nf -> (ctx, CommandOut (prettyDecoded nf))
execCommand ctx (CmdEqa a b) =
  case (resolve ctx a, resolve ctx b) of
    (Left err, _)        -> (ctx, CommandErr err)
    (_, Left err)        -> (ctx, CommandErr err)
    (Right a', Right b') -> (ctx, CommandEq (alphaEq a' b'))
execCommand ctx (CmdStep strat expr) =
  case resolve ctx expr of
    Left err -> (ctx, CommandErr err)
    Right e  -> (ctx, CommandStep (stepBeta strat ctx e))
execCommand ctx (CmdExplain expr) =
  (ctx, CommandExplain (explain (ctxEnv ctx) expr))
execCommand ctx (CmdType expr) =
  case resolve ctx expr of
    Left err -> (ctx, CommandErr err)
    Right e ->
      let tenv = if ctxTyped ctx then primTypes else []
          t = expandAll (ctxEnv ctx) e
      in  case inferType tenv t of
            Left err -> (ctx, CommandErr ("error: " ++ prettyTypeError prettyTypeGreek2 err))
            Right ty -> (ctx, CommandOut (prettyExpr e ++ " : " ++ prettyTypeGreek ty))

freeMsg :: Name -> [Name] -> String
freeMsg name frees =
  "error: ‘" ++ name ++ "’ has free variable"
    ++ plural ++ ": " ++ unwords frees
  where
    plural = if length frees == 1 then "" else "s"

-- | Replace an existing binding, or append a new one. Later lookups
-- (δ-reduction) see the new right-hand side.
setBind :: [(Name, Expr)] -> Name -> Expr -> [(Name, Expr)]
setBind [] name expr = [(name, expr)]
setBind ((n, e) : rest) name expr
  | n == name = (name, expr) : rest
  | otherwise = (n, e) : setBind rest name expr

-- | Do not unfold names. A hole in the term, or a name the term depends
-- on (directly or through other definitions) whose definition contains
-- a hole, is an error.
resolve :: Ctx -> Expr -> Either String Expr
resolve ctx expr
  | hasHole expr = Left "error: term contains a hole"
  | (name : _) <- [ n | n <- namesUsedBy env expr, Just def <- [lookup n env], hasHole def ] =
      Left ("error: hole in ‘" ++ name ++ "’")
  | otherwise = Right expr
  where
    env = ctxEnv ctx

rejectHole :: Expr -> Either String Expr
rejectHole e
  | hasHole e = Left "error: term contains a hole"
  | otherwise = Right e

followHole :: [Step] -> Either String [Step]
followHole steps =
  case [e | e <- terms, hasHole e] of
    [] -> Right steps
    _  -> Left "error: term contains a hole"
  where
    terms = concatMap stepTerms steps
    stepTerms (NoRedex e)         = [e]
    stepTerms (Stepped _ _ before after) = [before, after]

stripTrailingNewline :: String -> String
stripTrailingNewline s = reverse (dropWhile (== '\n') (reverse s))

renderResult :: Palette -> CommandResult -> String
renderResult pal result = case result of
  CommandQuit    -> ""
  CommandReload  -> ""
  CommandErr err -> withCode (palError pal) (palReset pal) err
  CommandOut s   -> s
  CommandEq True ->
    withCode (palOk pal) (palReset pal) "True"
  CommandEq False ->
    withCode (palFalse pal) (palReset pal) "False"
  CommandDef name expr ->
    name ++ " := " ++ prettyExpr expr
  CommandEnv binds -> renderEnv pal binds
  CommandStep step -> renderStep pal step
  CommandFollow steps -> renderFollow pal steps
  CommandExplain view -> renderExplain pal view

renderEnv :: Palette -> [(Name, Expr)] -> String
renderEnv pal [] =
  withCode (palMuted pal) (palReset pal) "(no bindings)"
renderEnv _ binds =
  intercalate "\n"
    [ name ++ " := " ++ prettyExpr expr
    | (name, expr) <- binds
    ]

-- | Show a redex the same way @:step@ does: color on a TTY, caret otherwise.
highlightRedex :: Palette -> RedexKind -> Path -> Expr -> String
highlightRedex pal kind path expr =
  let (term, span_) = prettyFocus path expr
      code = kindColor pal kind
  in  if null code
        then term ++ "\n" ++ prettyCaret span_
        else paint code (palReset pal) span_ term

kindColor :: Palette -> RedexKind -> String
kindColor pal Beta      = palBeta pal
kindColor pal (Delta _) = palDelta pal
kindColor pal (Prim _)  = palDelta pal

-- | The arrow of a chain line: @~b~>@ for β, @~K~>@ for unfolding @K@,
-- @~plus~>@ for a primitive.
arrowText :: RedexKind -> String
arrowText Beta      = "~b~>"
arrowText (Delta n) = "~" ++ n ++ "~>"
arrowText (Prim n)  = "~" ++ n ++ "~>"

-- | Blank space as wide as the arrow plus its trailing space, to keep a
-- caret line under the term it marks.
arrowPad :: RedexKind -> String
arrowPad kind = replicate (length (arrowText kind) + 1) ' '

renderArrow :: Palette -> RedexKind -> String
renderArrow pal kind =
  withCode (palMuted pal) (palReset pal) (arrowText kind)

renderStep :: Palette -> Step -> String
renderStep pal (NoRedex e) =
  prettyExpr e ++ "\n" ++ withCode (palMuted pal) (palReset pal) "no redex"
renderStep pal (Stepped kind path before after) =
  let shown = highlightRedex pal kind path before
      arrow = renderArrow pal kind
  in  shown ++ "\n" ++ arrow ++ " " ++ prettyExpr after

-- | A reduction trace: highlight each redex, then @~b~>@ / @~K~>@ the next term.
renderFollow :: Palette -> [Step] -> String
renderFollow pal steps = case steps of
  [] -> ""
  [NoRedex e] ->
    prettyExpr e ++ "\n" ++ withCode (palMuted pal) (palReset pal) "no redex"
  _ -> intercalate "\n" (go Nothing steps)
  where
    go _ [] = []
    go prev [NoRedex e] =
      [prefix prev (prettyExpr e)]
    go prev (Stepped kind path before _ : rest) =
      prefix prev (highlightRedex pal kind path before) : go (Just kind) rest
    go prev (NoRedex e : rest) =
      prefix prev (prettyExpr e) : go Nothing rest

    prefix Nothing shown = shown
    prefix (Just kind) shown =
      let arrow = renderArrow pal kind ++ " "
      in  case break (== '\n') shown of
            (term, '\n' : caret) ->
              arrow ++ term ++ "\n" ++ arrowPad kind ++ caret
            (term, _) ->
              arrow ++ term
