-- | Load a @.lam@ file (with imports) and run the checks of every task.
--
-- The result is a list of tasks, each with named checks that either
-- pass or fail with a message for the student. The homework template
-- turns this into HUnit tests; @lambda check@ prints it as a table.
module Lambda.Check
  ( CheckResult (..)
  , TaskResult (..)
  , TaskStatus (..)
  , Loaded (..)
  , Session (..)
  , loadFile
  , loadSession
  , checkFile
  , checkLoaded
  , taskStatus
  , prettyResults
  , reportOk
  , inhabitable
  , isHoleMessage
  ) where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (intercalate, isSuffixOf, nub, sort, (\\))
import Data.Maybe (catMaybes, fromMaybe, isJust)
import System.FilePath (takeDirectory, (</>), (<.>), takeExtension)

import Lambda.Decode (numeral, prettyDecoded)
import Lambda.Eval
import Lambda.Parser (churchNumeral, desugarNumerals, parseProgram)
import Lambda.Pretty (prettyExpr, prettyType, prettyTypeGreek, prettyTypeGreek2)
import Lambda.Subst
import Lambda.Syntax
import Lambda.Types

------------------------------------------------------------------------
-- Results
------------------------------------------------------------------------

data CheckResult = CheckResult
  { crName   :: String
  , crResult :: Either String ()
  } deriving (Eq, Show)

data TaskResult = TaskResult
  { trId     :: String
  , trTitle  :: String
  , trChecks :: [CheckResult]
  } deriving (Eq, Show)

data TaskStatus = Done | Partial Int Int | Failed
  deriving (Eq, Show)

taskStatus :: TaskResult -> TaskStatus
taskStatus tr =
  let total  = length (trChecks tr)
      passed = length [ () | CheckResult _ (Right ()) <- trChecks tr ]
  in  if total == 0 || passed == total then Done
      else if passed == 0 then Failed
      else Partial passed total

reportOk :: [TaskResult] -> Bool
reportOk = all ((== Done) . taskStatus)

------------------------------------------------------------------------
-- Loading
------------------------------------------------------------------------

-- | A loaded file: environment (imports first), symbolic variables,
-- the statements of the file itself, and its source lines.
data Loaded = Loaded
  { ldFile     :: FilePath
  , ldLanguage :: Language
  , ldVars     :: [Name]
  , ldEnv      :: [(Name, Expr)]      -- ^ all definitions, imports first
  , ldStmts    :: [Stmt]              -- ^ statements of this file
  , ldLines    :: [(Int, String)]
  , ldProblems :: [(Name, String)]    -- ^ definition-level problems
  } deriving (Eq, Show)

-- | What the REPL needs.
data Session = Session
  { sessionFile :: FilePath
  , sessionCtx  :: Ctx
  } deriving (Eq, Show)

loadSession :: FilePath -> IO (Either String Session)
loadSession path = fmap (Session path . ctxOf) <$> loadFile path

ctxOf :: Loaded -> Ctx
ctxOf ld = Ctx (ldEnv ld) (ldLanguage ld == Typed)

readFileEither :: FilePath -> IO (Either String String)
readFileEither path = do
  r <- try (readFile path) :: IO (Either IOException String)
  return $ either (const (Left ("lambda: не удаётся открыть ‘" ++ path ++ "’"))) Right r

-- | Parse a file, resolve imports (relative to the file), desugar
-- numerals, and collect definitions in order. Parse errors and import
-- errors are fatal; problems inside definitions are recorded.
loadFile :: FilePath -> IO (Either String Loaded)
loadFile = go []
  where
    go visited path
      | path `elem` visited = return (Left ("lambda: циклический import ‘" ++ path ++ "’"))
      | otherwise = do
          src <- readFileEither path
          case src of
            Left err -> return (Left err)
            Right text ->
              case parseProgram path text of
                Left err -> return (Left err)
                Right (Program stmts0) -> do
                  let lang = languageOf stmts0
                      stmts = map (desugarStmt lang) stmts0
                  imported <- importAll (path : visited) lang (takeDirectory path) [ (p, f) | SImport p f <- stmts ]
                  case imported of
                    Left err -> return (Left err)
                    Right envs -> do
                      let envImported = concat envs
                          vars = concat [ vs | SVars _ vs <- stmts ]
                          (env, problems) = collectDefs lang vars envImported stmts
                      return $ Right Loaded
                        { ldFile = path
                        , ldLanguage = lang
                        , ldVars = vars
                        , ldEnv = env
                        , ldStmts = stmts
                        , ldLines = zip [1 ..] (lines text)
                        , ldProblems = problems
                        }

    importAll _ _ _ [] = return (Right [])
    importAll visited lang dir ((pos, name) : rest) = do
      let file = if takeExtension name == ".lam" then dir </> name else dir </> name <.> "lam"
      r <- go visited file
      case r of
        Left err -> return (Left err)
        Right ld
          | ldLanguage ld /= lang ->
              return (Left (prettyPos pos ++ ": import ‘" ++ name ++ "’ написан на другом language"))
          | otherwise -> do
              others <- importAll visited lang dir rest
              return (fmap (ldEnv ld :) others)

languageOf :: [Stmt] -> Language
languageOf stmts = case [ l | SLanguage _ l <- stmts ] of
  (l : _) -> l
  [] -> Pure

desugarStmt :: Language -> Stmt -> Stmt
desugarStmt Typed s = s
desugarStmt Pure s = case s of
  SDef p n e -> SDef p n (d e)
  SExpect p a b -> SExpect p (d a) (d b)
  SNormal p a b -> SNormal p (d a) (d b)
  SRename p n e -> SRename p n (d e)
  SMinimal p n e raw -> SMinimal p n (d e) raw
  SChain p n o ls -> SChain p n o { chainFrom = fmap d (chainFrom o), chainTo = fmap d (chainTo o) }
                                  [ (a, d e) | (a, e) <- ls ]
  SChurch p n e -> SChurch p n (d e)
  SFamily p t e -> SFamily p t (d e)
  other -> other
  where d = desugarNumerals

-- | Definitions in order; later ones may use earlier ones. A definition
-- with unknown free names or a forward/self reference is still stored
-- (so later checks can name it) but recorded as a problem.
collectDefs :: Language -> [Name] -> [(Name, Expr)] -> [Stmt] -> ([(Name, Expr)], [(Name, String)])
collectDefs lang vars imported stmts = foldl step (imported, []) [ (p, n, e) | SDef p n e <- stmts ]
  where
    known env = map fst env ++ vars ++ (if lang == Typed then primNames else [])
    step (env, problems) (pos, name, expr) =
      let dup = [ prettyPos pos ++ ": повторное определение ‘" ++ name ++ "’" | name `elem` map fst env ]
          unknown = filter (`notElem` known env) (freeVars expr)
          selfRef = [ prettyPos pos ++ ": ‘" ++ name ++ "’ ссылается на себя (рекурсия не допускается)" | name `elem` unknown ]
          unknown' = unknown \\ [name]
          undefMsg = [ prettyPos pos ++ ": в ‘" ++ name ++ "’ не определено: " ++ unwords unknown' | not (null unknown') ]
          substMsg = [ prettyPos pos ++ ": подстановка [x |-> N] M допустима только первой строкой chain" | hasSubst expr ]
          arrowMsg = [ prettyPos pos ++ ": имя ‘" ++ name ++ "’ занято стрелкой ~" ++ name ++ "~>" | name `elem` arrowNames ]
          msgs = dup ++ selfRef ++ undefMsg ++ substMsg ++ arrowMsg
          env' = if name `elem` map fst env then env else env ++ [(name, expr)]
      in  (env', problems ++ [ (name, m) | m <- msgs ])

------------------------------------------------------------------------
-- Checking
------------------------------------------------------------------------

checkFile :: FilePath -> IO (Either String [TaskResult])
checkFile path = fmap (fmap checkLoaded) (loadFile path)

-- | Split statements into task sections and run each.
checkLoaded :: Loaded -> [TaskResult]
checkLoaded ld = map runTask (sections (ldStmts ld))
  where
    ctx = ctxOf ld
    runTask (tid, title, stmts) =
      let defs = nub [ n | SDef _ n _ <- stmts ]
          problems = [ CheckResult ("определение " ++ n) (Left m)
                     | n <- defs, (n', m) <- ldProblems ld, n == n' ]
          checks = concatMap (runStmt ld ctx) stmts
          holes = [ CheckResult ("определение " ++ n) (Left (holeMsg n))
                  | null checks, n <- defs, Just e <- [lookup n (ldEnv ld)], hasHole e ]
      in  TaskResult tid title (problems ++ holes ++ checks)

-- | Statements grouped by @task@ lines; statements before the first
-- task are the preamble and produce no task.
sections :: [Stmt] -> [(String, String, [Stmt])]
sections = go Nothing []
  where
    go current acc [] = close current acc []
    go current acc (STask _ tid title : rest) = close current acc (go (Just (tid, title)) [] rest)
    go current acc (s : rest) = go current (acc ++ [s]) rest
    close Nothing _ k = k
    close (Just (tid, title)) acc k = (tid, title, acc) : k

lineText :: Loaded -> SrcPos -> String
lineText ld pos = case lookup (posLine pos) (ldLines ld) of
  Just l -> stripComment (trim l)
  Nothing -> "строка " ++ show (posLine pos)
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    stripComment s = trimEnd (fst (breakComment s))
    trimEnd = reverse . dropWhile isSpace . reverse
    breakComment s = case s of
      [] -> ([], [])
      ('-' : '-' : _) -> ([], s)
      (c : cs) -> let (a, b) = breakComment cs in (c : a, b)

holeMsg :: Name -> String
holeMsg n = "в ‘" ++ n ++ "’ осталась дырка (...)"

answerHole :: String
answerHole = "ответ не дан (...)"

-- | Проверка упёрлась в дырку: к ней ещё не приступали, это не ошибка.
-- Все сообщения о дырках заканчиваются на @(...)@; раннер шаблона по этому
-- признаку отличает статус TODO от FAILED.
isHoleMessage :: String -> Bool
isHoleMessage = ("(...)" `isSuffixOf`)

-- | Names the expression depends on that still contain holes, or have
-- definition problems.
blockers :: Loaded -> Expr -> [String]
blockers ld e =
  let env = ldEnv ld
      deps = namesUsedBy env e
      holes = [ holeMsg n | n <- deps, Just d <- [lookup n env], hasHole d ]
      probs = [ m | n <- deps, (n', m) <- ldProblems ld, n == n' ]
      own = [ "в терме осталась дырка (...)" | hasHole e ]
      unknown = filter (`notElem` (map fst env ++ ldVars ld ++ prims)) (freeVars e)
      unk = [ "не определено: " ++ unwords unknown | not (null unknown) ]
      prims = if ldLanguage ld == Typed then primNames else []
  in  own ++ holes ++ probs ++ unk

guarded :: Loaded -> [Expr] -> Either String a -> Either String a
guarded ld exprs k = case concatMap (blockers ld) exprs of
  [] -> k
  (m : _) -> Left m

lookupDef :: Loaded -> Name -> Either String Expr
lookupDef ld n = case lookup n (ldEnv ld) of
  Just e -> Right e
  Nothing
    | ldLanguage ld == Typed && n `elem` primNames -> Right (Var n)
    | otherwise -> Left ("‘" ++ n ++ "’ не определено")

limit :: Int
limit = defaultLimit

runStmt :: Loaded -> Ctx -> Stmt -> [CheckResult]
runStmt ld ctx stmt = case stmt of
  SFree pos _ ["..."] -> one pos (Left answerHole)
  SType pos _ (Just (TVar "...")) -> one pos (Left answerHole)
  SInhabit pos _ _ ["..."] -> one pos (Left answerHole)
  SExpect pos a b -> one pos (checkExpect ld ctx a b)
  SNormal pos a b -> one pos (checkNormal ld ctx a b)
  SFree pos n xs -> one pos (checkFree ld n xs)
  SRename pos n e -> one pos (checkRename ld n e)
  SMinimal pos n e raw -> one pos (checkMinimal ld n e raw)
  SCheck pos n preds ->
    [ CheckResult (lineText ld pos ++ " → " ++ prettyPred p) (checkPred ld ctx n p) | p <- preds ]
  SChain pos n opts ls -> checkChain ld ctx (lineText ld pos) n opts ls
  SType pos n mt -> one pos (checkType ld ctx n mt)
  SChurch pos n e -> one pos (checkChurchStmt ld ctx n e)
  SInhabit pos t k ns -> checkInhabit ld ctx (lineText ld pos) t k ns
  SFamily pos t f -> checkFamily ld ctx (lineText ld pos) t f
  _ -> []
  where
    one pos r = [CheckResult (lineText ld pos) r]

prettyPred :: Predicate -> String
prettyPred p = case p of
  PClosed -> "closed"
  PWhnf -> "whnf"
  PHnf -> "hnf"
  PNf -> "nf"
  PNormalizing -> "normalizing"
  PUses x -> "uses " ++ x
  PAvoids x -> "avoids " ++ x
  PNot q -> "not " ++ prettyPred q

------------------------------------------------------------------------
-- expect / free / rename / minimal / check
------------------------------------------------------------------------

-- | Step budget for @expect@: enough for numeral arithmetic, small
-- enough that a diverging side is given up quickly.
expectLimit :: Int
expectLimit = 3000

-- | The message names the sides, not «получено / ожидалось»: which side holds
-- the student's answer depends on the task (@expect or true false = true@
-- against @expect K I S = ...@), so either fixed wording is wrong half the time.
checkExpect :: Loaded -> Ctx -> Expr -> Expr -> Either String ()
checkExpect ld ctx a b = guarded ld [a, b] $
  case betaEq ctx expectLimit a b of
    Equal -> Right ()
    Differ x y -> Left ("слева получается " ++ prettyDecoded x ++ ", а справа " ++ prettyDecoded y)
    Undecided -> Left "не удалось сравнить за лимит шагов (возможно, терм расходится)"

-- | @nf A = B@: the comparison of @expect@, and the answer @B@ has to be
-- written as a value. A term that still reduces (@pred 3@ for ⌜2⌝) is
-- β-equal to the answer, so @expect@ accepts it, but it is not the normal
-- form the task asks for. The form is checked first: the message about it
-- does not tell whether the term is equal to the answer.
checkNormal :: Loaded -> Ctx -> Expr -> Expr -> Either String ()
checkNormal ld ctx a b = guarded ld [a, b] $
  case unfinished ld ctx b of
    [] -> checkExpect ld ctx a b
    (t : _) -> Left ("справа не нормальная форма: ‘" ++ prettyExpr (digits t) ++ "’ ещё можно вычислить")
  where
    -- The answer the way it was written: numerals back to digits.
    digits e
      | ldLanguage ld == Pure, Just n <- numeral e = Lit n
      | otherwise = case e of
          Lam x t body -> Lam x t (digits body)
          App f arg -> App (digits f) (digits arg)
          _ -> e

-- | Subterms of an answer that are left to compute, outermost first; none
-- means the answer is written as a value. A value is
--
-- * a term without redexes once the names are unfolded: @I@, @true@, @3@, @\\z. y@;
-- * data built from values by a constructor: @pair 1 true@, @cons 4 (cons 9 nil)@,
--   @inl 3@ — unfolded, such a term has redexes, but they only put the
--   fields in place;
-- * a variable applied to values, an abstraction with a value for the body.
unfinished :: Loaded -> Ctx -> Expr -> [Expr]
unfinished ld ctx = go []
  where
    env = ldEnv ld
    ctx' = ctx { ctxEnv = [] }

    go bound e
      | isJust (findRedex Lazy ctx' (expandUnder bound e)) = inside bound e
      | otherwise = []

    inside bound e = case e of
      Lam x _ body -> go (x : bound) body
      App _ _ | (Var h, args) <- spine e ->
        let below = concatMap (go bound) args
        in  if accepts bound h (length args) then below
            -- a primitive waits for its arguments: @n + 2 * 3@ is stuck on @2 * 3@
            else if primitive bound h && not (null below) then below
            else [e]
      _ -> [e]

    -- The head takes this many arguments without anything to compute:
    -- a variable takes any number, a constructor no more than it has fields.
    accepts bound h n
      | h `elem` bound = True
      | Just def <- lookup h env = maybe False (n <=) (fields def)
      | otherwise = not (primitive bound h)

    primitive bound h = ldLanguage ld == Typed && h `elem` primNames && h `notElem` bound

    fields def = case normalize ctx' limit (expandAll env def) of
      Right t -> constructorFields (erase t)
      Left _ -> Nothing

    -- Names bound around the subterm are variables even if the file
    -- defines them: unfold under the same binders.
    expandUnder bound e = strip (length bound) (expandAll env (foldl (flip lam) e bound))
    strip :: Int -> Expr -> Expr
    strip n (Lam _ _ body) | n > 0 = strip (n - 1) body
    strip _ e = e

-- | The number of fields, if the term is a data constructor of a Church or
-- Scott encoding: @\\x1 .. xn h1 .. hm. hj a1 .. an@ — the fields, then the
-- handlers, and in the body one handler applied to the fields in order; a
-- recursive field is given the handlers first, as the tail in
-- @cons = \\x t f z. f x (t f z)@. So @pair@, @cons@, @inl@, @inr@ are
-- constructors whatever a homework calls them, and @K@, @plus@, @pred@ are not.
--
-- The successor has this shape too, but it is not counted: a number is
-- written in digits, and @suc (suc 1)@ is a computation left unfinished.
constructorFields :: Expr -> Maybe Int
constructorFields def
  | Var h <- hd
  , h `elem` handlers
  , nub params == params
  , length fieldNames == length args
  , and (zipWith field fieldNames args)
  , not (alphaEq def successor) = Just (length args)
  | otherwise = Nothing
  where
    (params, body) = binders' def
    (hd, args) = spine body
    (fieldNames, handlers) = splitAt (length args) params
    field x arg = arg == Var x || arg == apps (Var x) (map Var handlers)
    binders' (Lam x _ e) = let (xs, b) = binders' e in (x : xs, b)
    binders' e = ([], e)
    successor = lam "n" (lam "s" (lam "z" (App (Var "s") (apps (Var "n") [Var "s", Var "z"]))))

checkFree :: Loaded -> Name -> [Name] -> Either String ()
checkFree ld n xs = do
  e <- lookupDef ld n
  guarded ld [e] $
    let fv = sort (freeVars (expandAll (ldEnv ld) e))
        want = sort (nub xs)
    in  if fv == want then Right ()
        else Left ("свободные переменные: " ++ showSet fv ++ ", а не " ++ showSet want)
  where
    showSet [] = "(нет)"
    showSet ys = unwords ys

checkRename :: Loaded -> Name -> Expr -> Either String ()
checkRename ld n e = do
  given <- lookupDef ld n
  guarded ld [given, e] $
    if not (alphaEq (erase e) (erase given))
      then Left ("терм не α-эквивалентен ‘" ++ n ++ "’: " ++ prettyExpr given)
      else if not (barendregt e)
        then Left ("связыватели должны быть попарно различны и не совпадать со свободными переменными: "
                   ++ unwords (nub (binders e)))
        else Right ()

-- | The messages never print a term: the pretty-printer writes the minimal
-- form, which is the answer to the task.
checkMinimal :: Loaded -> Name -> Expr -> String -> Either String ()
checkMinimal ld n e raw = do
  given <- lookupDef ld n
  guarded ld [given, e] $
    if erase e /= erase given
      then if alphaEq (erase e) (erase given)
             then Left "имена связанных переменных менять не нужно"
             else Left ("это другой терм, не ‘" ++ n ++ "’: скобки расставлены так, что дерево изменилось")
      else
        let have = parenCount raw
            need = parenCount (prettyExpr e)
        in  if have > need
              then Left ("остались лишние скобки: можно убрать ещё " ++ pairs (have - need))
              else Right ()
  where
    pairs k = show k ++ " " ++ plural k ++ " скобок"
    plural k
      | k `mod` 10 == 1 && k `mod` 100 /= 11 = "пару"
      | k `mod` 10 `elem` [2, 3, 4] && k `mod` 100 `notElem` [12, 13, 14] = "пары"
      | otherwise = "пар"

checkPred :: Loaded -> Ctx -> Name -> Predicate -> Either String ()
checkPred ld ctx n p = do
  def <- lookupDef ld n
  guarded ld [def] $
    let t = expandAll (ldEnv ld) def
        ctx' = ctx { ctxEnv = [] }
    in  if holds t ctx' def p then Right () else Left (failMsg t p)
  where
    holds t ctx' def q = case q of
      PClosed -> null (freeVars t)
      PWhnf -> isWhnf t
      PHnf -> isHnf t
      PNf -> isNf t
      PNormalizing -> normalizing ctx' limit t
      PUses x -> x `elem` namesUsedBy (ldEnv ld) def
      PAvoids x -> x `notElem` namesUsedBy (ldEnv ld) def
      PNot r -> not (holds t ctx' def r)
    failMsg t q = case q of
      PClosed -> "терм не замкнут, свободны: " ++ unwords (freeVars t)
      PWhnf -> "терм не в WHNF: " ++ prettyExpr t
      PHnf -> "терм не в HNF: " ++ prettyExpr t
      PNf -> "терм не в нормальной форме: " ++ prettyExpr t
      PNormalizing -> "терм не нормализуется за лимит шагов (сильно нормализуемым он не выглядит)"
      PUses x -> "определение не использует ‘" ++ x ++ "’"
      PAvoids x -> "определение использует ‘" ++ x ++ "’, а это запрещено"
      PNot r -> "предикат ‘" ++ prettyPred r ++ "’ выполняется, а не должен: " ++ prettyExpr t

------------------------------------------------------------------------
-- Chains
------------------------------------------------------------------------

checkChain :: Loaded -> Ctx -> String -> Name -> ChainOpts -> [(Maybe Arrow, Expr)] -> [CheckResult]
checkChain _ _ label _ _ [] = [CheckResult label (Left "цепочка пуста")]
checkChain ld ctx label _ opts ls@((_, firstTerm) : restLines) =
  case concatMap (blockers ld) (map snd ls ++ catMaybes [chainFrom opts, chainTo opts]) of
    (m : _) -> [CheckResult label (Left m)]
    [] -> fromCheck ++ stepChecks ++ endCheck
  where
    env = ldEnv ld
    expand = expandAll env
    strategy = chainStrategy opts
    terms = map snd ls
    lastTerm = snd (last ls)

    named suffix = label ++ ", " ++ suffix

    fromCheck = case chainFrom opts of
      Nothing -> []
      Just g ->
        let given = resolveName g
            ok = alphaEq (erase (expand firstTerm)) (erase (expand given))
        in  [CheckResult (named "начало") $
              if ok then Right () else Left ("первая строка должна быть " ++ prettyExpr given)]

    endCheck = case chainTo opts of
      Just target ->
        let t = resolveName target
            ok = alphaEq (erase (expand lastTerm)) (erase (expand t))
        in  [CheckResult (named "конец") $
              if ok then Right () else Left ("последняя строка должна быть эквивалентна " ++ prettyExpr t
                                             ++ ", а это " ++ prettyExpr lastTerm)]
      Nothing ->
        let lastT = expand lastTerm
        in  [CheckResult (named "конец") $
              if isNf lastT then Right ()
              else Left ("в последнем терме остались редексы после раскрытия имён: " ++ prettyExpr lastT)]

    -- A `to` / `from` argument may be a name of a definition.
    resolveName e = case e of
      Var x | Just d <- lookup x env -> d
      _ -> e

    stepChecks = zipWith3 stepCheck [2 :: Int ..] terms restLines

    stepCheck i prev (marrow, cur) =
      let arrow = fromMaybe ArrBeta marrow
          name = named ("строка " ++ show i)
      in  CheckResult name (stepOk i prev arrow cur)

    stepOk i prev arrow cur = case arrow of
      ArrSubst -> case prev of
        Subst bs m ->
          let result = substAll bs m
          in  if alphaEq (erase result) (erase cur) then Right ()
              else Left ("подстановка даёт " ++ prettyExpr result ++ ", а не " ++ prettyExpr cur)
        _ -> Left "~s~> допустима только после строки вида [x |-> N] M"
      ArrAlpha ->
        if alphaEq (erase prev) (erase cur) || (isStrategy && alphaEq (erase (expand prev)) (erase (expand cur)))
          then Right ()
          else Left ("строки " ++ show (i - 1) ++ " и " ++ show i ++ " не α-эквивалентны")
      ArrEta ->
        let oneEta from to = any (alphaEq (erase to)) (etaContractions (erase from))
            sides = [(prev, cur)] ++ [ (expand prev, expand cur) | isStrategy ]
        in  if or [ oneEta a b || oneEta b a | (a, b) <- sides ] then Right ()
            else Left ("строка " ++ show i ++ " не получается из строки " ++ show (i - 1)
                       ++ " одним η-шагом: \\x. M x ~eta~> M, если x не свободна в M (можно и в обратную сторону)")
      ArrDelta name
        | name `notElem` map fst env ->
            Left ("‘" ++ name ++ "’ не определено" ++ legacyDelta name)
        | any (alphaEq cur) (unfoldingsOf ctx name prev) || any (alphaEq prev) (unfoldingsOf ctx name cur) ->
            Right ()
        | otherwise ->
            case nub (unfoldedNames prev cur ++ unfoldedNames cur prev) of
              (other : _) ->
                Left ("строка " ++ show i ++ " получается из строки " ++ show (i - 1)
                      ++ " раскрытием ‘" ++ other ++ "’, а не ‘" ++ name ++ "’")
              [] ->
                Left ("строка " ++ show i ++ " не получается из строки " ++ show (i - 1)
                      ++ " раскрытием одного вхождения ‘" ++ name ++ "’" ++ hint (unfoldingsOf ctx name prev))
      ArrMany ->
        if isStrategy then Left "~~> нельзя использовать в цепочке со стратегией"
        else if any (alphaEq cur) (reachable ctx (chainSteps opts) prev) then Right ()
        else Left ("строка " ++ show i ++ " не достигается из строки " ++ show (i - 1)
                   ++ " за " ++ show (chainSteps opts) ++ " шагов")
      ArrBeta -> case strategy of
        Nothing ->
          let options = [ contractBeta p prev | p <- betaPaths prev ]
          in  if any (alphaEq cur) options then Right ()
              else if any (alphaEq (expand cur)) [ contractBeta p (expand prev) | p <- betaPaths (expand prev) ]
                then Left "шаг верен только после раскрытия имени: сначала раскройте его отдельной строкой ~имя~>"
                else Left ("строка " ++ show i ++ " не получается из строки " ++ show (i - 1)
                           ++ " одним β-шагом" ++ hint options)
        Just s ->
          let strat = case s of
                ChainNormal -> Lazy
                ChainApplicative -> Applicative
              ctx' = ctx { ctxEnv = [] }
              p = expand prev
          in  case stepBeta strat ctx' p of
                NoRedex _ -> Left ("в строке " ++ show (i - 1) ++ " нет редексов")
                Stepped _ _ _ after ->
                  if alphaEq (erase after) (erase (expand cur)) then Right ()
                  else Left ("это не шаг стратегии " ++ stratName s ++ "; ожидалось: " ++ prettyExpr after)

    isStrategy = isJust strategy
    stratName ChainNormal = "normal"
    stratName ChainApplicative = "applicative"

    -- Names whose single unfolding turns @from@ into @to@.
    unfoldedNames from to = [ n | (n, e) <- namedUnfoldings ctx from, alphaEq e to ]

    -- The v2 draft wrote every unfolding as ~d~>; point old files at the new form.
    legacyDelta "d" = "; раскрытие имени пишется с самим именем: ~K~>, ~S~>, …"
    legacyDelta _ = ""

    hint [] = ""
    hint options = "; возможные шаги: " ++ intercalate " | " (map prettyExpr (take 3 options))

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

typeEnv :: Loaded -> [(Name, Type)]
typeEnv ld = if ldLanguage ld == Typed then primTypes else []

typeErr :: TypeError -> String
typeErr = prettyTypeError prettyTypeGreek2

checkType :: Loaded -> Ctx -> Name -> Maybe Type -> Either String ()
checkType ld _ n want = do
  def <- lookupDef ld n
  guarded ld [def] $
    let t = expandAll (ldEnv ld) def
    in  case (inferType (typeEnv ld) t, want) of
          (Left _, Nothing) -> Right ()
          (Left err, Just _) -> Left ("терм не типизируется: " ++ typeErr err)
          (Right ty, Nothing) -> Left ("терм типизируется: " ++ prettyTypeGreek ty)
          (Right ty, Just w)
            | typesEqualUpToRenaming ty w -> Right ()
            | isInstanceOf w ty -> Left ("тип " ++ prettyType w ++ " верен, но не наиболее общий; наиболее общий: " ++ prettyTypeGreek ty)
            | otherwise -> Left ("наиболее общий тип: " ++ prettyTypeGreek ty ++ ", а не " ++ prettyType w)

checkChurchStmt :: Loaded -> Ctx -> Name -> Expr -> Either String ()
checkChurchStmt ld _ n e = do
  given <- lookupDef ld n
  guarded ld [given] $
    let g = expandAll (ldEnv ld) given
        tenv = typeEnv ld
    in  if hasHole e then Left "в терме осталась дырка (...)"
        else if not (alphaEq (erase e) (erase g))
          then Left ("после стирания аннотаций получается " ++ prettyExpr (erase e) ++ ", а нужно " ++ prettyExpr g)
          else case checkChurch tenv e of
            Left err -> Left ("терм не типизируется по аннотациям: " ++ typeErr err)
            Right ty -> case inferType tenv (erase g) of
              Left err -> Left ("исходный терм не типизируется: " ++ typeErr err)
              Right principal
                | typesEqualUpToRenaming ty principal -> Right ()
                | otherwise -> Left ("аннотации дают тип " ++ prettyTypeGreek ty
                                     ++ ", а наиболее общий: " ++ prettyTypeGreek principal)

checkInhabit :: Loaded -> Ctx -> String -> Type -> Maybe Int -> [Name] -> [CheckResult]
checkInhabit ld ctx label ty wanted names
  | null names =
      [CheckResult label $
        if inhabitableWith literals 4 ty then Left ("тип " ++ prettyType ty ++ " населён, обитатель есть") else Right ()]
  | otherwise =
      let each = [ CheckResult (label ++ " → " ++ n) (inhabitant ld ctx ty n) | n <- names ]
          distinctCheck = [ CheckResult (label ++ " → попарно различны") (distinct ld ctx names) | length names > 1 ]
          -- @inhabit T (k)@: the task asks for at least k inhabitants
          countCheck = [ CheckResult (label ++ " → не меньше " ++ show k ++ " обитателей") $
                           if length names >= k then Right ()
                           else Left ("перечислено " ++ show (length names) ++ ", а нужно " ++ show k)
                       | Just k <- [wanted] ]
      in  each ++ distinctCheck ++ countCheck
  where
    literals = if ldLanguage ld == Typed then [TCon "Int", TCon "Bool"] else []

inhabitant :: Loaded -> Ctx -> Type -> Name -> Either String ()
inhabitant ld _ ty n = do
  def <- lookupDef ld n
  guarded ld [def] $ inhabitantTerm ld ty (expandAll (ldEnv ld) def)

inhabitantTerm :: Loaded -> Type -> Expr -> Either String ()
inhabitantTerm ld ty t
  | not (null free) = Left ("терм не замкнут, свободны: " ++ unwords free)
  | otherwise = case inferType (typeEnv ld) t of
      Left err -> Left ("терм не типизируется: " ++ typeErr err)
      Right p
        | isInstanceOf ty p -> Right ()
        | otherwise -> Left ("тип терма " ++ prettyTypeGreek p ++ " не обобщает " ++ prettyType ty)
  where
    free = freeVars t \\ map fst (typeEnv ld)

distinct :: Loaded -> Ctx -> [Name] -> Either String ()
distinct ld ctx names = do
  defs <- mapM (lookupDef ld) names
  guarded ld defs $ do
    forms <- mapM normal (zip names defs)
    case [ (a, b) | (i, (a, x)) <- zip [0 :: Int ..] forms, (j, (b, y)) <- zip [0 ..] forms, i < j, alphaEq x y ] of
      [] -> Right ()
      ((a, b) : _) -> Left ("‘" ++ a ++ "’ и ‘" ++ b ++ "’ αβη-эквивалентны")
  where
    normal (n, d) = case normalize ctx limit d of
      Left _ -> Left ("‘" ++ n ++ "’ не нормализуется за лимит шагов")
      Right nf -> Right (n, etaReduce (erase nf))

checkFamily :: Loaded -> Ctx -> String -> Type -> Expr -> [CheckResult]
checkFamily ld ctx label ty f =
  case blockers ld f of
    (m : _) -> [CheckResult label (Left m)]
    [] ->
      let members = [ (k, normalize ctx limit (App f (churchNumeral k))) | k <- [0 .. 4] ]
          each = [ CheckResult (label ++ " → член " ++ show k) $ case r of
                     Left _ -> Left ("F ⌜" ++ show k ++ "⌝ не нормализуется за лимит шагов")
                     Right t -> inhabitantTerm ld ty (erase t)
                 | (k, r) <- members ]
          forms = [ (k, etaReduce (erase t)) | (k, Right t) <- members ]
          dup = [ (a, b) | (a, x) <- forms, (b, y) <- forms, a < b, alphaEq x y ]
          distinctCheck = CheckResult (label ++ " → члены попарно различны") $ case dup of
            [] -> Right ()
            ((a, b) : _) -> Left ("F ⌜" ++ show a ++ "⌝ и F ⌜" ++ show b ++ "⌝ αβη-эквивалентны")
      in  each ++ [distinctCheck]

-- | Bounded search for a closed pure lambda-term in long normal form.
inhabitable :: Int -> Type -> Bool
inhabitable = inhabitableWith []

-- Closed constants can be used alongside bound variables during the search.
inhabitableWith :: [Type] -> Int -> Type -> Bool
inhabitableWith constants depth0 = go depth0 constants
  where
    go depth ctx ty
      | depth <= 0 = False
      | otherwise =
          let (args, result) = unarrow ty
              ctx' = args ++ ctx
          in  any (useVar depth ctx' result) ctx'
    useVar depth ctx result h =
      let (hargs, hresult) = unarrow h
      in  hresult == result && all (go (depth - 1) ctx) hargs
    unarrow (TArr a b) = let (as, r) = unarrow b in (a : as, r)
    unarrow t = ([], t)

------------------------------------------------------------------------
-- Text report
------------------------------------------------------------------------

prettyResults :: [TaskResult] -> String
prettyResults = intercalate "\n" . map one
  where
    one tr =
      let status = case taskStatus tr of
            Done -> "DONE"
            Partial k n -> "PARTIAL " ++ show k ++ "/" ++ show n
            Failed -> "FAILED"
          title = if null (trTitle tr) then "" else " " ++ trTitle tr
          failures = [ "    " ++ crName c ++ "\n      " ++ m | c@(CheckResult _ (Left m)) <- trChecks tr ]
      in  intercalate "\n" (("task " ++ trId tr ++ title ++ ": " ++ status) : failures)
