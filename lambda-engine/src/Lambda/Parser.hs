-- | Line-oriented parser for @.lam@ files (format v2, see FORMAT.md).
--
-- Each statement lives on one line; a @chain@ continues on indented
-- lines. Expressions may contain spaces and trailing @--@ comments, but
-- not newlines.
module Lambda.Parser
  ( parseProgram
  , parseExpr
  , parseType
  , parseWith
  , Parser
  , sc
  , pExpr
  , pAtom
  , pType
  , ident
  , keyword
  , symbol
  , churchNumeral
  , desugarNumerals
  ) where

import Control.Monad (void, when)
import Data.Char (isAlphaNum, isDigit, isLetter, isUpper)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Lambda.Syntax

type Parser = Parsec Void String

-- | Skip spaces, tabs, and @--@ comments; do not consume newlines.
sc :: Parser ()
sc = L.space
  (void (some (char ' ' <|> char '\t')))
  (L.skipLineComment "--")
  empty

-- | Like 'sc', but also skip blank lines (used between statements).
scn :: Parser ()
scn = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reserved :: [String]
reserved =
  [ "language", "import", "vars", "task", "rule", "expect", "nf", "free", "rename"
  , "minimal", "check", "chain", "type", "church", "inhabit", "family" ]

------------------------------------------------------------------------
-- Entry points
------------------------------------------------------------------------

-- | Parse a whole file. The file name is used in error messages and
-- positions. Numerals are left as 'Lit'; call 'desugarNumerals' for
-- @language pure@.
parseProgram :: FilePath -> String -> Either String Program
parseProgram file src =
  case runParser (scn *> many pStmt <* eof) file src of
    Left bundle -> Left (errorBundlePretty bundle)
    Right stmts -> Right (Program stmts)

-- | Parse a single expression (tests, REPL).
parseExpr :: String -> Either String Expr
parseExpr = parseWith "<expr>" pExpr

parseType :: String -> Either String Type
parseType = parseWith "<type>" pType

-- | Run a parser on a single line, skipping leading spaces/comments.
parseWith :: String -> Parser a -> String -> Either String a
parseWith name p src =
  case runParser (sc *> p <* eof) name src of
    Left bundle -> Left (errorBundlePretty bundle)
    Right x     -> Right x

------------------------------------------------------------------------
-- Statements
------------------------------------------------------------------------

pStmt :: Parser Stmt
pStmt = do
  pos <- srcPos
  stmt <- choice
    [ keyword "language" *> pLanguage pos
    , keyword "import"   *> (SImport pos <$> restOfLineWord)
    , keyword "vars"     *> (SVars pos <$> many ident)
    , keyword "task"     *> pTask pos
    , keyword "rule"     *> (SRule pos <$> restOfLine)
    , keyword "expect"   *> (SExpect pos <$> pExpr <* symbol "=" <*> pExpr)
    , keyword "nf"       *> (SNormal pos <$> pExpr <* symbol "=" <*> pExpr)
    , keyword "free"    *> (SFree pos <$> ident <* symbol "=" <*> pNamesOrHole)
    , keyword "rename"   *> (SRename pos <$> ident <* symbol "=" <*> pExpr)
    , keyword "minimal"  *> pMinimal pos
    , keyword "check"    *> (SCheck pos <$> ident <* symbol ":" <*> pPredicate `sepBy1` symbol ",")
    , keyword "chain"    *> pChain pos
    , keyword "type"     *> (SType pos <$> ident <* symbol "=" <*> pTypeOrNone)
    , keyword "church"   *> (SChurch pos <$> ident <* symbol "=" <*> pExpr)
    , keyword "inhabit"  *> (SInhabit pos <$> pType <*> optional (parens (lexeme L.decimal)) <* symbol ":" <*> pNamesOrNone)
    , keyword "family"   *> (SFamily pos <$> pType <* symbol ":" <*> pExpr)
    , pDef pos
    ]
  case stmt of
    SChain {} -> return ()
    _         -> endStmt
  return stmt

pLanguage :: SrcPos -> Parser Stmt
pLanguage pos = do
  name <- ident
  case name of
    "pure"  -> return (SLanguage pos Pure)
    "typed" -> return (SLanguage pos Typed)
    _       -> fail ("unknown language ‘" ++ name ++ "’ (use pure or typed)")

pTask :: SrcPos -> Parser Stmt
pTask pos = do
  tid  <- lexeme (some (satisfy (\c -> isAlphaNum c || c == '.' || c == '_')))
  STask pos tid <$> restOfLine

pMinimal :: SrcPos -> Parser Stmt
pMinimal pos = do
  name <- ident
  _    <- symbol "="
  (raw, expr) <- match pExpr
  return (SMinimal pos name expr raw)

-- | @name := term@, the practice's «обозначим». A plain @=@ is the
-- most likely slip, so it gets its own message.
pDef :: SrcPos -> Parser Stmt
pDef pos = do
  name <- ident
  op   <- symbol ":=" <|> symbol "="
  when (op == "=") $
    fail ("a definition is written with ‘:=’: " ++ name ++ " := …")
  SDef pos name <$> pExpr

-- | An unanswered @type@ is written @...@ and parsed as the type
-- variable @...@; the checker reports it as a hole.
pTypeOrNone :: Parser (Maybe Type)
pTypeOrNone = (Nothing <$ keyword "none") <|> (Just (TVar "...") <$ symbol "...") <|> (Just <$> pType)

pNamesOrNone :: Parser [Name]
pNamesOrNone = ([] <$ keyword "none") <|> pNamesOrHole

-- | A list of names — comma-separated (@p1, p2@), the commas may be omitted — or
-- @...@ for "not answered yet".
pNamesOrHole :: Parser [Name]
pNamesOrHole = (["..."] <$ symbol "...") <|> many (ident <* optional (symbol ","))

-- | The predicate @nf@ shares its spelling with the statement @nf A = B@, so
-- the name of a predicate is read as a plain word, not as an identifier.
pPredicate :: Parser Predicate
pPredicate = do
  name <- lexeme (some (satisfy identChar)) <?> "predicate"
  case name of
    "not"         -> PNot <$> pPredicate
    "closed"      -> return PClosed
    "whnf"        -> return PWhnf
    "hnf"         -> return PHnf
    "nf"          -> return PNf
    "normalizing" -> return PNormalizing
    "uses"        -> PUses <$> ident
    "avoids"      -> PAvoids <$> ident
    _ -> fail ("unknown predicate ‘" ++ name ++ "’")

-- | @chain NAME [from ATOM] [to ATOM] [(steps N)] [(strategy S)] =@ then
-- indented lines.
pChain :: SrcPos -> Parser Stmt
pChain pos = do
  name <- ident
  opts <- chainOpts defaultChainOpts
  _    <- symbol "="
  endStmtKeepIndent
  first <- chainLine True
  rest  <- many (chainLine False)
  scn
  return (SChain pos name opts (first : rest))
  where
    chainOpts o = choice
      [ keyword "from" *> pAtom >>= \e -> chainOpts o { chainFrom = Just e }
      , keyword "to"   *> pAtom >>= \e -> chainOpts o { chainTo = Just e }
      , try (symbol "(" *> keyword "steps") *> L.decimal <* symbol ")" >>= \n ->
          sc *> chainOpts o { chainSteps = fromInteger n }
      , try (symbol "(" *> keyword "strategy") *> ident <* symbol ")" >>= \s ->
          case s of
            "normal"      -> chainOpts o { chainStrategy = Just ChainNormal }
            "applicative" -> chainOpts o { chainStrategy = Just ChainApplicative }
            _ -> fail ("unknown strategy ‘" ++ s ++ "’ (use normal or applicative)")
      , return o
      ]

    -- A chain line: indentation, optional arrow, expression, end of line.
    -- Only the indentation (and, after the first line, the sight of an
    -- arrow) is backtracked; from there on a mistake inside the line
    -- is reported where it is, not as "expected a statement".
    chainLine isFirst = do
      _ <- try $ do
        skipBlankLines
        indent <- some (char ' ' <|> char '\t')
        if isFirst then return () else void (lookAhead (char '~' <|> char '='))
        return indent
      arrow <- if isFirst then return Nothing else Just <$> pArrow
      expr  <- pChainExpr
      endLine
      return (arrow, expr)

    pChainExpr = pSubst <|> pExpr

    skipBlankLines = void $ many (try (sc *> eol))

-- | @~b~>@, @~s~>@, @~eta~>@, @~~>@, @=a=@, or @~NAME~>@ for unfolding a
-- definition (the lecture's ⇝ with the name as subscript). @b@, @s@ and
-- @eta@ are taken by arrows, see 'arrowNames'.
pArrow :: Parser Arrow
pArrow = lexeme $ choice
  [ ArrMany  <$ try (string "~~>")
  , ArrAlpha <$ try (string "=a=")
  , try named <?> "arrow (~b~>, ~NAME~>, ~eta~>, =a=, ~~>, ~s~>)"
  ]
  where
    named = do
      _ <- char '~'
      first <- satisfy identChar
      rest <- many (satisfy identChar)
      _ <- string "~>"
      let name = first : rest
      case name of
        "b" -> return ArrBeta
        "s" -> return ArrSubst
        "eta" -> return ArrEta
        _ | all isDigit name ->
              fail ("‘~" ++ name ++ "~>’: numerals are already unfolded, no step is needed")
          | identStart first -> return (ArrDelta name)
          | otherwise -> fail ("‘~" ++ name ++ "~>’ is not an arrow")

-- | @[x |-> N, y |-> M] body@ — simultaneous substitution (the
-- practice's @[x ↦ N] M@).
pSubst :: Parser Expr
pSubst = do
  _ <- symbol "["
  binds <- pBind `sepBy1` symbol ","
  _ <- symbol "]"
  Subst binds <$> pExpr
  where
    pBind = do
      x <- ident
      op <- symbol "|->" <|> symbol ":="
      when (op == ":=") $
        fail "a substitution is written [x |-> N] M; ‘:=’ is for definitions"
      n <- pExpr
      return (x, n)

-- | The rest of the line without the trailing comment, trimmed.
restOfLine :: Parser String
restOfLine = do
  s <- takeWhileP (Just "text") (\c -> c /= '\n' && c /= '\r')
  return (trim (beforeComment s))
  where
    trim = dropWhileEnd' (== ' ') . dropWhile (== ' ')
    dropWhileEnd' p = reverse . dropWhile p . reverse
    beforeComment str = case str of
      [] -> []
      ('-' : '-' : _) -> []
      (c : cs) -> c : beforeComment cs

restOfLineWord :: Parser String
restOfLineWord = lexeme (some (satisfy (`notElem` " \t\r\n")))

-- | A statement ends at newline or end of file. Following blank lines
-- and comment-only lines are skipped.
endStmt :: Parser ()
endStmt = eof <|> (void eol *> scn)

endStmtKeepIndent :: Parser ()
endStmtKeepIndent = eof <|> void eol

endLine :: Parser ()
endLine = eof <|> void eol

srcPos :: Parser SrcPos
srcPos = do
  p <- getSourcePos
  return SrcPos
    { posFile = sourceName p
    , posLine = unPos (sourceLine p)
    , posCol  = unPos (sourceColumn p)
    }

------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

pExpr :: Parser Expr
pExpr = pLam <|> pOps

-- | Infix operators are sugar for applications of the primitive names:
-- @x + 1@ is @plus x 1@ (see 'infixOps'). Precedence, from tightest:
-- application, @*@, @+ -@, @< ==@; all left-associative. The right operand
-- may be a lambda without parentheses, as in application.
pOps :: Parser Expr
pOps = level [("<", "lt"), ("==", "eq")]
     $ level [("+", "plus"), ("-", "minus")]
     $ level [("*", "mult")] pApps
  where
    level ops operand = operand >>= go
      where
        go x = (do n <- choice [ n <$ pOperator s | (s, n) <- ops ]
                   y <- operand <|> pLam
                   go (App (App (Var n) x) y))
               <|> pure x

-- | An operator token, not the prefix of a longer one: @-@ is not @->@,
-- @=@ alone (the @=@ of @expect A = B@) is not @==@.
pOperator :: String -> Parser ()
pOperator s = lexeme . try $ string s *> notFollowedBy (oneOf "-=<>+*")

-- | @\\x y. body@, @λx y. body@ or @\\x y -> body@; binders may carry
-- Church annotations @x:T@.
pLam :: Parser Expr
pLam = do
  _    <- symbol "\\" <|> symbol "λ"
  xs   <- some pBinder
  _    <- symbol "." <|> symbol "->"
  body <- pExpr
  return (foldr (\(x, t) b -> Lam x t b) body xs)

pBinder :: Parser (Name, Maybe Type)
pBinder = do
  x <- ident
  t <- optional (try (symbol ":") *> pTypeAtom)
  return (x, t)

-- | Left-associative application. A trailing lambda is allowed as an
-- argument without parentheses: @f \\x. x@.
pApps :: Parser Expr
pApps = do
  f    <- pAtom
  args <- many (pAtom <|> pLam)
  return (foldl App f args)

pAtom :: Parser Expr
pAtom =
      pHole
  <|> pNumber
  <|> (Var <$> ident)
  <|> parens (pSubst <|> pExpr)

pHole :: Parser Expr
pHole = do
  pos <- srcPos
  _   <- symbol "..."
  return (Hole pos)

pNumber :: Parser Expr
pNumber = Lit <$> lexeme L.decimal

ident :: Parser Name
ident = lexeme . try $ do
  first <- satisfy identStart <?> "identifier"
  rest  <- many (satisfy identChar)
  let name = first : rest
  when (name `elem` reserved) $
    fail ("‘" ++ name ++ "’ is a keyword")
  return name

identStart :: Char -> Bool
identStart c = (isLetter c && c /= 'λ') || c == '_'

identChar :: Char -> Bool
identChar c = (isAlphaNum c && c /= 'λ') || c == '_' || c == '\''

-- | A reserved word. The token must not be the prefix of an identifier.
keyword :: String -> Parser String
keyword k = lexeme . try $ string k <* notFollowedBy (satisfy identChar)

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

pType :: Parser Type
pType = do
  t <- pTypeAtom
  rest <- optional (symbol "->" *> pType)
  return (maybe t (TArr t) rest)

-- | Type constants are @Int@ and @Bool@; other capitalised names are
-- rejected rather than silently read as type variables.
pTypeAtom :: Parser Type
pTypeAtom = parens pType <|> tyName
  where
    tyName = do
      name <- ident
      case name of
        _ | name `elem` ["Int", "Bool"] -> return (TCon name)
        c : _ | isUpper c ->
          fail ("unknown type constant ‘" ++ name ++ "’ (only Int and Bool; type variables are lowercase)")
        _ -> return (TVar name)

------------------------------------------------------------------------
-- Numerals
------------------------------------------------------------------------

churchNumeral :: Integer -> Expr
churchNumeral n = lam "s" (lam "z" (iterate (App (Var "s")) (Var "z") !! fromInteger n))

-- | In @language pure@, integer literals stand for Church numerals.
desugarNumerals :: Expr -> Expr
desugarNumerals = go
  where
    go (Lit n)       = churchNumeral n
    go (Var x)       = Var x
    go (Lam x t e)   = Lam x t (go e)
    go (App f a)     = App (go f) (go a)
    go h@(Hole _)    = h
    go (Subst bs m)  = Subst [ (x, go n) | (x, n) <- bs ] (go m)
