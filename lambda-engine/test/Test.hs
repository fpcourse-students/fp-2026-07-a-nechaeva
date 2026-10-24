-- | Tests of the checker: HUnit for the examples, QuickCheck for the laws,
-- the same pair the homework runner uses.
module Main (main) where

import Control.Monad (filterM)
import Data.List (isInfixOf, isPrefixOf, (\\))
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import Test.QuickCheck qualified as QC

import Lambda
import Lambda.Check (isHoleMessage)
import Lambda.Color (ColorMode (..), Palette (..), paint, parseColorMode, plainPalette, withCode)
import Lambda.Console (setupConsole)
import Lambda.Eval (normalize, parseStrategy, prettyStrategy)
import Lambda.Need (normalizeNeed)
import Lambda.Pretty (prettyTypeGreek2)
import Lambda.Subst (barendregt, binders, etaContractions, hasHole, parenCount)
import Lambda.Syntax (Arrow (..), ChainOpts (..), Predicate (..))
import Lambda.Types (prettyTypeError, primTypes)

main :: IO ()
main = do
  setupConsole
  files <- fileTests
  errorFiles <- errorFileTests
  inhabitants <- inhabitantFileTests
  result <- runTestTT $ TestList
    [ parseTests, evalTests, substTests, typeTests, decodeTests, prettyTests
    , colorTests, explainTests, commandTests, propertyTests, files, errorFiles, inhabitants ]
  if errors result + failures result == 0 then exitSuccess else exitFailure

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | The test data lives next to this package; inside a homework repository
-- (package lambda-engine/) tests may run from that repository's root instead.
dataFile :: FilePath -> IO FilePath
dataFile name = do
  let candidates = [ prefix ++ name | prefix <- ["", "lambda-engine/"] ]
  existing <- filterM doesFileExist candidates
  return (case existing of
    (found : _) -> found
    [] -> name)

group :: String -> [Test] -> Test
group name = TestLabel name . TestList

eq :: (Eq a, Show a) => String -> a -> a -> Test
eq label expected actual = label ~: expected ~=? actual

ok :: String -> Bool -> Test
ok label = (label ~:) . assertBool label

-- | A QuickCheck property as one HUnit case, like Test.Properties of the runner.
prop :: QC.Testable p => String -> p -> Test
prop name p = name ~: TestCase $ do
  result <- QC.quickCheckWithResult QC.stdArgs { QC.chatty = False, QC.maxSuccess = 150 } p
  case result of
    QC.Success {} -> return ()
    other -> assertFailure ("property failed:\n" ++ QC.output other)

mustParse :: String -> Expr
mustParse src = case parseExpr src of
  Left err -> error ("parse failed: " ++ src ++ "\n" ++ err)
  Right e -> e

pure' :: String -> Expr
pure' = desugarNumerals . mustParse

nfOf :: String -> String
nfOf src = case normalForm defaultLimit (pure' src) of
  Left TooManySteps -> error ("step limit: " ++ src)
  Right e -> prettyExpr e

isLeft' :: Either a b -> Bool
isLeft' = either (const True) (const False)

leftHas :: String -> Either String b -> Bool
leftHas s = either (s `isInfixOf`) (const False)

unlinesNoEnd :: [String] -> String
unlinesNoEnd = foldr1 (\a b -> a ++ "\n" ++ b)

-- | The lecture's definitions, as a REPL / evaluation context.
lectureEnv :: [(Name, Expr)]
lectureEnv =
  [ (n, pure' e)
  | (n, e) <-
      [ ("I", "\\x. x"), ("K", "\\x y. x"), ("K'", "\\x y. y"), ("S", "\\x y z. x z (y z)")
      , ("true", "\\a b. a"), ("false", "\\a b. b"), ("pair", "\\a b p. p a b")
      , ("suc", "\\n s z. s (n s z)"), ("plus", "\\n m s z. n s (m s z)"), ("mult", "\\n m s. n (m s)")
      , ("twice", "\\f a. f (f a)")
      , ("omega", "(\\x. x x) (\\x. x x)")
      ]
  ]

------------------------------------------------------------------------
-- Parsing and statements
------------------------------------------------------------------------

parseTests :: Test
parseTests = group "parse"
  [ eq "dot" (Lam "x" Nothing (Var "x")) (mustParse "\\x. x")
  , eq "arrow" (Lam "x" Nothing (Var "x")) (mustParse "\\x -> x")
  , eq "unicode lambda" (Lam "x" Nothing (Lam "y" Nothing (Var "y"))) (mustParse "λx y. y")
  , eq "app assoc" (App (App (Var "f") (Var "x")) (Var "y")) (mustParse "f x y")
  , eq "trailing lambda" (App (Var "f") (Lam "x" Nothing (Var "x"))) (mustParse "f \\x. x")
  , eq "annotation" (Lam "x" (Just (TArr (TVar "a") (TVar "a"))) (Var "x")) (mustParse "\\x:(a -> a). x")
  , eq "literal" (Lit 3) (mustParse "3")
  , eq "numeral" (churchNumeral 2) (pure' "2")
  -- infix operators are sugar for the primitive names
  , eq "infix plus" (App (App (Var "plus") (Var "x")) (Lit 1)) (mustParse "x + 1")
  , eq "infix precedence" (App (App (Var "plus") (Lit 1)) (App (App (Var "mult") (Lit 2)) (Lit 3))) (mustParse "1 + 2 * 3")
  , eq "infix left assoc" (App (App (Var "minus") (App (App (Var "minus") (Var "a")) (Var "b"))) (Var "c")) (mustParse "a - b - c")
  , eq "infix below application" (App (App (Var "lt") (App (Var "f") (Var "x"))) (Lit 2)) (mustParse "f x < 2")
  , eq "infix eq" (App (App (Var "eq") (Var "x")) (Lit 0)) (mustParse "x == 0")
  , eq "minus is not an arrow" (Lam "x" Nothing (App (App (Var "minus") (Var "x")) (Lit 1))) (mustParse "\\x -> x - 1")
  , eq "infix parens" (App (App (Var "mult") (App (App (Var "plus") (Var "x")) (Lit 1))) (Lit 2)) (mustParse "(x + 1) * 2")
  , case prog "expect x + 1 = 2\n" of
      Right [SExpect _ a b] -> eq "infix before =" (App (App (Var "plus") (Var "x")) (Lit 1), Lit 2) (a, b)
      other -> eq "infix before =" "expect" (show other)
  , case prog "nf K I S = I\n" of
      Right [SNormal _ a b] -> eq "nf statement" (App (App (Var "K") (Var "I")) (Var "S"), Var "I") (a, b)
      other -> eq "nf statement" "nf" (show other)
  , case prog "check t: nf, not nf\n" of
      Right [SCheck _ _ ps] -> eq "nf is still a predicate" [PNf, PNot PNf] ps
      other -> eq "nf is still a predicate" "check" (show other)
  , ok "nf is a keyword" (isLeft' (parseExpr "nf x"))
  , ok "a name may start with nf" (mustParse "nfoo x" == App (Var "nfoo") (Var "x"))
  , case prog "inhabit a -> a -> a (2): p1, p2\n" of
      Right [SInhabit _ _ k ns] -> eq "inhabit count" (Just 2, ["p1", "p2"]) (k, ns)
      other -> eq "inhabit count" "inhabit" (show other)
  , case prog "inhabit a -> a: p1 p2 p3\n" of
      Right [SInhabit _ _ k ns] -> eq "inhabit names without commas" (Nothing, ["p1", "p2", "p3"]) (k, ns)
      other -> eq "inhabit names without commas" "inhabit" (show other)
  , eq "subst" (Subst [("x", Var "S")] (App (Var "x") (Var "y"))) (mustParse "([x |-> S] x y)")
  , eq "subst2" (Subst [("x", Var "S"), ("y", Var "K")] (Var "x")) (mustParse "([x |-> S, y |-> K] x)")
  , ok "subst with := rejected" (isLeft' (parseExpr "([x := S] x)"))
  , eq "repl define" (Right (CmdBind "K" (Lam "x" Nothing (Lam "y" Nothing (Var "x"))))) (parseCommand "K := \\x y. x")
  , ok "repl define with = rejected" (leftHas "written with ‘:=’" (parseCommand "K = \\x y. x"))
  , ok "file define with = rejected" (leftHas "written with ‘:=’" (parseProgram "<t>" "K = \\x y. x\n"))
  , eq "prime names" (Var "K'") (mustParse "K'")
  , ok "hole" (case mustParse "..." of Hole _ -> True; _ -> False)
  , ok "keyword rejected" (isLeft' (parseExpr "expect x"))
  , eq "pretty" "\\x y. x (y z)" (prettyExpr (mustParse "\\x. \\y. x (y z)"))
  , eq "pretty parens" "(\\x. x) (\\y. y)" (prettyExpr (mustParse "(\\x. x) (\\y. y)"))
  , eq "pretty annotation" "\\x:(a -> b) y:a. x y" (prettyExpr (mustParse "\\x:(a -> b) y:a. x y"))
  , eq "type parse" (TArr (TArr (TVar "a") (TVar "b")) (TVar "a")) (either error id (parseType "(a -> b) -> a"))
  , eq "type pretty" "(a -> b) -> a -> b" (prettyType (either error id (parseType "(a -> b) -> (a -> b)")))
  , ok "unknown type constant" (leftHas "unknown type constant ‘Nat’" (parseType "Nat -> a"))
  , eq "type constants" (Right (TArr (TCon "Int") (TCon "Bool"))) (parseType "Int -> Bool")
  , eq ":reload" (Right CmdReload) (parseCommand ":reload")
  , eq ":r" (Right CmdReload) (parseCommand ":r")
  , eq ":r result" CommandReload (runInput (pureCtx []) ":r")
  -- statements
  , ok "unknown language" (leftHas "unknown language ‘foo’" (prog "language foo\n"))
  , ok "unknown predicate" (leftHas "unknown predicate ‘foo’" (prog "check x: foo\n"))
  , ok "unknown chain strategy" (leftHas "unknown strategy" (prog "chain c (strategy eager) =\n  x\n"))
  , ok "numeral arrow" (leftHas "numerals are already unfolded" (prog "chain c =\n  3\n  ~3~> x\n"))
  , ok "chain error is positioned inside the line" (leftHas "4:" (prog "vars x\nchain c =\n  x\n  ~b~> (x\n"))
  , case prog "task 1.2 (title here)\nchain c to I (steps 2) =\n  x\n\n  =a= x\nexpect x = x\n" of
      Right [STask _ tid title, SChain _ n opts ls, SExpect {}] -> group "chain statement"
        [ eq "task id" "1.2" tid
        , eq "task title" "(title here)" title
        , eq "chain name" "c" n
        , eq "chain steps" 2 (chainSteps opts)
        , eq "chain to" (Just (Var "I")) (chainTo opts)
        , eq "chain lines" [Nothing, Just ArrAlpha] (map fst ls)
        ]
      other -> eq "statements" "task, chain, expect" (show other)
  , case prog "type t = ...\nfree f = ...\ntype u = none\ninhabit a: none\n" of
      Right [SType _ _ t1, SFree _ _ names, SType _ _ t2, SInhabit _ _ _ inh] -> group "hole statements"
        [ eq "type hole" (Just (TVar "...")) t1
        , eq "free hole" ["..."] names
        , eq "type none" Nothing t2
        , eq "inhabit none" [] inh
        ]
      other -> eq "hole statements" "type, free, type, inhabit" (show other)
  ]
  where
    prog src = fmap (\(Program ss) -> ss) (parseProgram "<t>" src)

------------------------------------------------------------------------
-- Evaluation
------------------------------------------------------------------------

evalTests :: Test
evalTests = group "eval"
  [ eq "id id" "\\y. y" (nfOf "(\\x. x) (\\y. y)")
  , eq "capture" "\\y'. y" (nfOf "(\\x. \\y. x) y")
  , eq "numerals" "\\s z. s (s (s z))" (nfOf "(\\n m s z. n s (m s z)) 1 2")
  , ok "omega diverges" (isLeft' (normalForm 100 (pure' "(\\x. x x) (\\x. x x)")))
  , eq "delta" "x" (either (error . show) prettyExpr (normalFormWith Lazy ctx 100 (pure' "K x I")))
  , eq "alpha" True (alphaEq (pure' "\\x. x y") (pure' "\\z. z y"))
  , eq "alpha free" False (alphaEq (pure' "\\x. x y") (pure' "\\z. z w"))
  , eq "beta paths" 2 (length (betaPaths (pure' "(\\x. x) ((\\y. y) z)")))
  , eq "contract beta" "z" (prettyExpr (contractBeta Here (pure' "(\\y. y) z")))
  , eq "unfoldings" 2 (length (unfoldings ctx (pure' "K I")))
  , eq "named unfoldings" ["K", "I"] (map fst (namedUnfoldings ctx (pure' "K I")))
  , eq "unfoldingsOf" "(\\x y. x) I" (concatMap prettyExpr (unfoldingsOf ctx "K" (pure' "K I")))
  , eq "bound name is not unfolded" [] (unfoldings ctx (pure' "\\K. K x"))
  , eq "delta step names" (Just "K") (case stepBeta Lazy ctx (pure' "K x") of
      Stepped (Delta n) _ _ _ -> Just n
      _ -> Nothing)
  , eq "expand" "\\x y. x" (prettyExpr (expandAll (ctxEnv ctx) (Var "K")))
  , eq "eta" "f" (prettyExpr (etaReduce (pure' "\\x. f x")))
  , eq "eta contractions: the outer redex, then the inner one" ["\\f y. f y", "\\f x. f x"]
      (map prettyExpr (etaContractions (pure' "\\f. (\\x. (\\y. f y) x)")))
  , eq "no eta when the variable is used" [] (etaContractions (pure' "\\x. f x x"))
  , eq "need: shared argument" (Just "\\s z. s (s (s (s z)))")
      (fmap prettyExpr (normalizeNeed [] 1000 (pure' "(\\n s z. n s (n s z)) 2")))
  , eq "need: capture" (Just "\\y'. y") (fmap prettyExpr (normalizeNeed [] 1000 (pure' "(\\x. \\y. x) y")))
  , eq "need: definitions" (Just "x") (fmap prettyExpr (normalizeNeed (ctxEnv ctx) 1000 (pure' "K x I")))
  , eq "need: omega runs out of fuel" Nothing (normalizeNeed [] 1000 (pure' "(\\x. x x) (\\x. x x)"))
  , eq "need: agrees with normal order under binders" (Right "\\a. a (\\b. b)")
      (fmap prettyExpr (normalize (pureCtx []) 100 (pure' "\\a. a ((\\c. c) (\\b. b))")))
  -- strategies
  , eq "normal step" "y" (afterStep Lazy term)
  , eq "applicative step" "(\\x. y) w" (afterStep Applicative term)
  , eq "strict step" "(\\x. y) w" (afterStep Strict term)
  , eq "strategy names" [Just Lazy, Just Lazy, Just Strict, Just Applicative, Nothing]
      (map parseStrategy ["lazy", "normal", "strict", "applicative", "eager"])
  , eq "strategy pretty" ["normal", "strict", "applicative"] (map prettyStrategy [Lazy, Strict, Applicative])
  , eq "no redex in nf" Nothing (findRedex Lazy ctx (pure' "\\x. x y"))
  -- traces
  , case followSteps Lazy ctx 100 (pure' "K x (I y)") of
      Right steps -> group "follow"
        [ eq "length" 4 (length steps)
        , eq "ends in nf" (NoRedex (Var "x")) (last steps)
        , eq "kinds" [Just (Delta "K"), Just Beta, Just Beta, Nothing]
            [ case s of Stepped k _ _ _ -> Just k; NoRedex _ -> Nothing | s <- steps ]
        ]
      Left err -> eq "follow" "Right" (show err)
  , eq "follow limit" (Left TooManySteps) (followSteps Lazy ctx 10 (pure' "(\\x. x x) (\\x. x x)"))
  -- shapes
  , ok "whnf" (isWhnf (pure' "\\x. (\\y. y) x"))
  , ok "not hnf" (not (isHnf (pure' "\\x. (\\y. y) x")))
  , ok "hnf" (isHnf (pure' "\\x. x ((\\y. y) x)"))
  , ok "not nf" (not (isNf (pure' "\\x. x ((\\y. y) x)")))
  , ok "normalizing" (normalizing ctx 1000 (pure' "K I I"))
  , ok "not normalizing" (not (normalizing ctx 1000 (pure' "(\\x. x x) (\\x. x x)")))
  -- β-equivalence
  , eq "betaEq nf" Equal (betaEq ctx 1000 (pure' "K x y") (pure' "x"))
  , eq "betaEq eta" Equal (betaEq ctx 1000 (pure' "\\x. f x") (pure' "f"))
  , eq "betaEq Y" Equal (betaEq ctxY 2000 (pure' "F m") (pure' "m F"))
  , ok "betaEq differ" (case betaEq ctx 1000 (pure' "K x y") (pure' "y") of Differ _ _ -> True; _ -> False)
  , ok "betaEq undecided" (case betaEq ctx 200 (pure' "(\\x. x x) (\\x. x x)") (pure' "x") of Undecided -> True; _ -> False)
  , eq "reachable" True (any (alphaEq (pure' "x")) (reachable ctx 3 (pure' "K x (I I)")))
  , eq "reachable zero steps" 1 (length (reachable ctx 0 (pure' "K x (I I)")))
  ]
  where
    ctx = pureCtx [("I", pure' "\\x. x"), ("K", pure' "\\x y. x")]
    ctxY = pureCtx
      [ ("Y", pure' "\\f. (\\x. f (x x)) (\\x. f (x x))")
      , ("g", pure' "\\f m. m f")
      , ("F", pure' "Y g") ]
    term = pure' "(\\x. y) ((\\z. z) w)"
    afterStep strat e = case stepBeta strat ctx e of
      Stepped _ _ _ after -> prettyExpr after
      NoRedex _ -> "no redex"

------------------------------------------------------------------------
-- Substitution and syntactic checks
------------------------------------------------------------------------

substTests :: Test
substTests = group "subst"
  [ eq "subst capture" "\\y'. y" (prettyExpr (subst "x" (Var "y") (pure' "\\y. x")))
  , eq "subst shadowed" "\\x. x" (prettyExpr (subst "x" (Var "y") (pure' "\\x. x")))
  , eq "freeVars order" ["y", "x"] (freeVars (pure' "y (\\z. x z) y"))
  , eq "erase" "\\x. x" (prettyExpr (erase (mustParse "\\x:a. x")))
  , eq "eta keeps used var" "\\x. f x x" (prettyExpr (etaReduce (pure' "\\x. f x x")))
  , eq "eta nested" "f" (prettyExpr (etaReduce (pure' "\\x y. f x y")))
  , ok "hasHole" (hasHole (pure' "\\x. x ..."))
  , ok "no hole" (not (hasHole (pure' "\\x. x")))
  , eq "binders" ["x", "y"] (binders (pure' "\\x. \\y. x"))
  , ok "barendregt" (barendregt (pure' "\\x. \\y. x z"))
  , ok "not barendregt: repeated binder" (not (barendregt (pure' "\\x. \\x. x")))
  , ok "not barendregt: binder equals free" (not (barendregt (pure' "\\x. x y (\\y. y)")))
  , eq "parenCount" 2 (parenCount "(a (b))")
  ]

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

typeTests :: Test
typeTests = group "types"
  [ eq "id" "α -> α" (infer "\\x. x")
  , eq "K" "α -> β -> α" (infer "\\x y. x")
  , eq "S" "(α -> β -> γ) -> (α -> β) -> α -> γ" (infer "\\x y z. x z (y z)")
  , eq "annotation kept" "(a -> a) -> a -> a" (infer "\\x:(a -> a). x")
  , ok "omega untypable" (isLeft' (inferType [] (mustParse "\\x. x x")))
  , eq "occurs message" "бесконечный тип: α ∼ α -> β" (errText (inferType [] (mustParse "\\x. x x")))
  , eq "unbound" (Left (UnboundVar "q")) (inferType [] (Var "q"))
  , eq "unbound message" "переменная ‘q’ не определена" (errText (inferType [] (Var "q")))
  , eq "hole" (Left HoleInTerm) (inferType [] (mustParse "..."))
  , eq "hole message" "в терме осталась дырка" (prettyTypeError prettyTypeGreek2 HoleInTerm)
  , eq "missing annotation message" "связыватель ‘x’ без аннотации типа" (prettyTypeError prettyTypeGreek2 (MissingAnnotation "x"))
  , eq "not allowed message" "что-то" (prettyTypeError prettyTypeGreek2 (NotAllowed "что-то"))
  , ok "renaming" (typesEqualUpToRenaming (ty "a -> b -> a") (ty "b -> a -> b"))
  , ok "not renaming" (not (typesEqualUpToRenaming (ty "a -> b -> a") (ty "a -> a -> a")))
  , ok "renaming with constants" (typesEqualUpToRenaming (ty "Int -> a") (ty "Int -> b"))
  , ok "constants differ" (not (typesEqualUpToRenaming (ty "Int -> a") (ty "Bool -> a")))
  , ok "instance" (isInstanceOf (ty "a -> a -> a") (ty "a -> b -> a"))
  , ok "not instance" (not (isInstanceOf (ty "a -> b -> a") (ty "a -> a -> a")))
  , eq "church" (Right "a -> a") (fmap prettyTypeGreek (checkChurch [] (mustParse "(\\x:(a -> a). x) (\\y:a. y)")))
  , eq "church missing annotation" (Left (MissingAnnotation "x")) (checkChurch [] (mustParse "\\x. x"))
  , ok "church mismatch" (case checkChurch [] (mustParse "(\\x:a. x) (\\y:a. y)") of Left (CannotUnify _ _) -> True; _ -> False)
  , ok "inhabited" (inhabitable 4 (ty "(a -> a) -> a -> a"))
  , ok "empty type" (not (inhabitable 4 (ty "a")))
  , ok "peirce not inhabited" (not (inhabitable 4 (ty "((a -> b) -> a) -> a")))
  -- primitives of language typed
  , eq "prim type" (Just (ty "Int -> Int -> Int")) (lookup "plus" primTypes)
  , eq "literal type" "Int" (either (error . show) prettyTypeGreek (inferType primTypes (Lit 3)))
  , eq "prim application" "Int" (either (error . show) prettyTypeGreek (inferType primTypes (mustParse "plus 1 2")))
  , ok "prim mismatch" (case inferType primTypes (mustParse "plus true") of Left (CannotUnify _ _) -> True; _ -> False)
  , ok "prim mismatch message" ("не удаётся унифицировать" `isInfixOf` errText (inferType primTypes (mustParse "plus true")))
  ]
  where
    ty = either error id . parseType
    infer src = either (error . show) prettyTypeGreek (inferType [] (mustParse src))
    errText = either (prettyTypeError prettyTypeGreek2) (const "no error")

------------------------------------------------------------------------
-- Decoder and printer
------------------------------------------------------------------------

decodeTests :: Test
decodeTests = group "decode"
  [ eq "numeral" "⌜3⌝" (prettyDecoded (pure' "3"))
  , eq "true" "true" (prettyDecoded (pure' "\\a b. a"))
  , eq "false is also zero" "⌜0⌝" (prettyDecoded (pure' "\\a b. b"))
  , eq "pair" "⟨⌜1⌝, ⌜2⌝⟩" (prettyDecoded (pure' "\\p. p 1 2"))
  , eq "nested pair" "⟨⌜1⌝, ⟨true, ⌜0⌝⟩⟩" (prettyDecoded (pure' "\\p. p 1 (\\q. q (\\a b. a) (\\a b. b))"))
  , eq "list" "[⌜1⌝, ⌜2⌝]" (prettyDecoded (pure' "\\c n. c 1 (c 2 n)"))
  , eq "list of pairs" "[⟨⌜1⌝, ⌜2⌝⟩]" (prettyDecoded (pure' "\\c n. c (\\p. p 1 2) n"))
  , eq "plain" "\\x. x" (prettyDecoded (pure' "\\x. x"))
  , eq "stuck application" "K x" (prettyDecoded (pure' "K x"))
  , eq "numeral argument" "f ⌜2⌝" (prettyDecoded (pure' "f 2"))
  , eq "lambda argument" "f (\\x. x y)" (prettyDecoded (pure' "f (\\x. x y)"))
  , eq "literal" "3" (prettyDecoded (Lit 3))
  , eq "hole" "..." (prettyDecoded (mustParse "..."))
  ]

prettyTests :: Test
prettyTests = group "pretty"
  [ eq "focus arg" ("f (g x)", (2, 5)) (prettyFocus (InArg Here) (pure' "f (g x)"))
  , eq "focus fun" ("f (g x)", (0, 1)) (prettyFocus (InFun Here) (pure' "f (g x)"))
  , eq "focus body" ("\\x. x y", (4, 3)) (prettyFocus (InBody Here) (pure' "\\x. x y"))
  , eq "focus whole" ("f x", (0, 3)) (prettyFocus Here (pure' "f x"))
  , eq "infix round trip" "x + 1" (prettyExpr (mustParse "x + 1"))
  , eq "infix precedence printed" "1 + 2 * 3" (prettyExpr (mustParse "plus 1 (mult 2 3)"))
  , eq "infix parens printed" "(x + 1) * 2" (prettyExpr (mustParse "mult (plus x 1) 2"))
  , eq "infix right assoc parens" "a - (b - c)" (prettyExpr (mustParse "minus a (minus b c)"))
  , eq "infix under application" "if (x < 2) (x + 1) 0" (prettyExpr (mustParse "if (lt x 2) (plus x 1) 0"))
  , eq "unsaturated stays prefix" "plus 1" (prettyExpr (mustParse "plus 1"))
  , eq "focus infix right" ("x + f y", (4, 3)) (prettyFocus (InArg Here) (mustParse "x + f y"))
  , eq "focus infix left" ("f y + x", (0, 3)) (prettyFocus (InFun (InArg Here)) (mustParse "f y + x"))
  , eq "caret" "  ^~~~~" (prettyCaret (2, 5))
  , eq "caret single" "^" (prettyCaret (0, 1))
  , eq "caret clamps" "^" (prettyCaret (-1, 0))
  , eq "pretty subst" "[x |-> S] x y" (prettyExpr (mustParse "([x |-> S] x y)"))
  , eq "pretty subst under app" "f ([x |-> S] x)" (prettyExpr (App (Var "f") (mustParse "([x |-> S] x)")))
  , eq "pretty literal" "3" (prettyExpr (Lit 3))
  , eq "pretty hole" "..." (prettyExpr (mustParse "..."))
  , eq "pretty lambda arg" "f (\\x. x) y" (prettyExpr (mustParse "f (\\x. x) y"))
  , eq "type constants" "Int -> Bool" (prettyType (TArr (TCon "Int") (TCon "Bool")))
  -- only generated variables (named ?…) are renamed to greek letters
  , eq "greek" "α -> β" (prettyTypeGreek (TArr (TVar "?9") (TVar "?2")))
  , eq "greek keeps user names" "q -> α" (prettyTypeGreek (TArr (TVar "q") (TVar "?1")))
  , eq "greek pair renames consistently" ("α", "α -> β") (prettyTypeGreek2 (TVar "?b") (TArr (TVar "?b") (TVar "?a")))
  ]

colorTests :: Test
colorTests = group "color"
  [ eq "color modes" [Just ColorAuto, Just ColorAlways, Just ColorNever, Just ColorAlways, Just ColorNever, Just ColorAlways, Just ColorNever, Nothing]
      (map parseColorMode ["auto", "ALWAYS", "never", "on", "off", "yes", "no", "maybe"])
  , eq "withCode plain" "s" (withCode "" ">" "s")
  , eq "withCode" "<s>" (withCode "<" ">" "s")
  , eq "paint plain" "abcdefg" (paint "" ">" (2, 3) "abcdefg")
  , eq "paint" "ab<cde>fg" (paint "<" ">" (2, 3) "abcdefg")
  , eq "paint empty span" "abc" (paint "<" ">" (1, 0) "abc")
  , eq "plain palette is empty" [] (filter (not . null) (palBinders plainPalette ++ [palReset plainPalette, palBeta plainPalette]))
  ]

explainTests :: Test
explainTests = group "explain"
  [ eq "pretty" "\\x y. x (K y)" (evPretty v)
  , eq "binders" 2 (evBinders v)
  , eq "free marks env" [("K", True)] (evFree v)
  , eq "no hole" False (evHole v)
  , eq "no def" Nothing (evDef v)
  , eq "def" (Just ("K", pure' "\\x y. x")) (evDef (explain lectureEnv (Var "K")))
  , eq "hole" True (evHole (explain lectureEnv (pure' "\\x. x ...")))
  , eq "free order" [("y", False), ("z", False)] (evFree (explain lectureEnv (pure' "y (\\x. z x)")))
  , eq "spans per bound occurrence" 3 (length (evSpans (explain lectureEnv (pure' "\\x. x x"))))
  ]
  where
    v = explain lectureEnv (pure' "\\x y. x (K y)")

------------------------------------------------------------------------
-- REPL commands
------------------------------------------------------------------------

commandTests :: Test
commandTests = group "command"
  [ group "terms"
      [ eq "term to nf" "x" (out "K x y")
      , eq "numerals in terms" "\\s z. s (s (s z))" (out "suc 2")
      , eq ":nf with strategy" "x" (out ":nf strict K x (I y)")
      , eq "strict evaluates a named argument first" "error: did not reach normal form (reduction limit)" (out ":nf strict K x omega")
      , eq "normal order skips it" "x" (out "K x omega")
      , eq "unknown strategy word is read as a term" "foo x" (out ":nf foo x")
      , eq "reduction limit" "error: did not reach normal form (reduction limit)" (out "omega")
      , eq ":decode" "⌜3⌝" (out ":decode suc 2")
      , eq ":decode nested" "⟨⌜1⌝, ⟨true, ⌜0⌝⟩⟩" (out ":decode pair 1 (pair true false)")
      ]
  , group "traces"
      [ eq ":step" "K x (I y)\n^\n~K~> (\\x y. x) x (I y)" (out ":step K x (I y)")
      , eq ":step no redex" "x\nno redex" (out ":step x")
      , eq ":follow" (unlinesNoEnd
          [ "K x (I y)"
          , "^"
          , "~K~> (\\x y. x) x (I y)"
          , "     ^~~~~~~~~~~"
          , "~b~> (\\y. x) (I y)"
          , "     ^~~~~~~~~~~~~"
          , "~b~> x"
          ]) (out ":follow K x (I y)")
      , eq ":follow strict" (unlinesNoEnd
          [ "K x (I y)"
          , "^"
          , "~K~> (\\x y. x) x (I y)"
          , "     ^~~~~~~~~~~"
          , "~b~> (\\y. x) (I y)"
          , "              ^"
          , "~I~> (\\y. x) ((\\x. x) y)"
          , "             ^~~~~~~~~~~"
          , "~b~> (\\y. x) y"
          , "     ^~~~~~~~~"
          , "~b~> x"
          ]) (out ":follow strict K x (I y)")
      , eq ":follow no redex" "x\nno redex" (out ":follow x")
      , eq ":follow limit" "error: did not reach normal form (reduction limit)" (out ":follow omega")
      ]
  , group ":explain"
      [ eq "tree" (unlinesNoEnd
          [ "\\x. x (y x)"
          , ""
          , "free  y"
          , ""
          , "\\ x"
          , "`-- @"
          , "    |-- x"
          , "    `-- @"
          , "        |-- y"
          , "        `-- x"
          ]) (out ":explain \\x. x (y x)")
      , eq "env names" (unlinesNoEnd
          [ "S K K"
          , ""
          , "free  S (env), K (env)"
          , ""
          , "@"
          , "|-- @"
          , "|   |-- S"
          , "|   `-- K"
          , "`-- K"
          ]) (out ":explain S K K")
      , eq "definition" "K\n\nfree  K (env)\nK := \\x y. x\n\nK" (out ":explain K")
      , eq "hole" (unlinesNoEnd
          [ "\\x. x ..."
          , ""
          , "free  (none)"
          , "hole  ..."
          , ""
          , "\\ x"
          , "`-- @"
          , "    |-- x"
          , "    `-- ..."
          ]) (out ":explain \\x. x ...")
      ]
  , group "definitions"
      [ eq "define echo" "three := suc (\\s z. s (s z))" (render r1)
      , eq "defined name is usable" "⌜3⌝" (render (runInput ctx1 ":decode three"))
      , eq "defined name is last in :env" "three := suc (\\s z. s (s z))" (last (lines (render (runInput ctx1 ":env"))))
      , eq "redefinition is used" "y" (render (runInput ctx2 "K x y"))
      , eq "redefinition keeps the order" (map fst lectureEnv) (map fst (ctxEnv ctx2))
      , eq ":env empty" "(no bindings)" (render (runInput (pureCtx []) ":env"))
      , eq "free variable in definition" "error: ‘f’ has free variable: q" (errOf "f := \\x. q x")
      , eq "free variables in definition" "error: ‘f’ has free variables: q r" (errOf "f := \\x. q x r")
      , eq "self reference" "error: ‘f’ has free variable: f" (errOf "f := f")
      , eq "definition with =" "error: a definition is written with ‘:=’: f := …" (errOf "f = \\x. x")
      , eq "reload keeps the context" True (fst (runLine ctx ":r") == ctx)
      ]
  , group "holes"
      [ eq "hole definition echo" "h := ..." (render rH)
      , eq "hole name" "error: hole in ‘h’" (outH "h")
      , eq "hole under application" "error: hole in ‘h’" (outH "h x")
      , eq "hole through another name" "error: hole in ‘h’" (outH "h2")
      , eq "hole in the term" "error: term contains a hole" (outH "(\\x. x) ...")
      , eq "hole in :follow" "error: hole in ‘h’" (outH ":follow K h x")
      , eq "hole in :decode" "error: hole in ‘h’" (outH ":decode h")
      , eq "hole in :type" "error: hole in ‘h’" (outH ":type h")
      , eq "hole in :step" "error: hole in ‘h’" (outH ":step h")
      , eq "hole in :eq-a" "error: hole in ‘h’" (outH ":eq-a h h")
      , ok "hole definition in :explain is shown" ("h := ..." `isInfixOf` outH ":explain h")
      , ok "hole term in :explain is shown" ("hole  ..." `isInfixOf` outH ":explain h ...")
      ]
  , group "alpha and types"
      [ eq ":eq-a true" "True" (out ":eq-a (\\x y. x) (\\a b. a)")
      , eq ":eq-a false" "False" (out ":eq-a (\\x. x y) (\\y. y y)")
      , eq ":eq-a does not unfold names" "False" (out ":eq-a K (\\a b. a)")
      , eq ":eq-a needs parentheses" "error: unexpected \"\\x.\"; expecting \"...\", '(', identifier, or integer" (errOf ":eq-a \\x. x \\y. y")
      , eq ":type" "K : α -> β -> α" (out ":type K")
      , eq ":type unfolds names" "twice : (α -> α) -> α -> α" (out ":type twice")
      , eq ":type error" "error: бесконечный тип: α ∼ α -> β" (out ":type omega")
      ]
  , group "commands"
      [ eq "unknown command" "error: unknown command ‘:foo’ (try :help)" (errOf ":foo")
      , eq "bare colon" "error: expected a command after ‘:’ (try :help)" (errOf ":")
      , ok "parse error is one line" (length (lines (errOf "(\\x. x")) == 1 && "error: " `isPrefixOf` errOf "(\\x. x")
      , ok ":help" ("A term by itself" `isPrefixOf` out ":help")
      , eq ":h alias" (out ":help") (out ":h")
      , eq ":quit" CommandQuit (runInput ctx ":quit")
      , eq ":q" CommandQuit (runInput ctx ":q")
      , eq "quit renders empty" "" (render CommandQuit)
      ]
  , group "language typed"
      [ eq "primitive" "3" (outT "plus 1 2")
      , eq "literal stays a literal" "3" (outT "3")
      , eq ":decode" "true" (outT ":decode true")
      , eq ":step" "1 + 2 * 3\n    ^~~~~\n~mult~> 1 + 6" (outT ":step plus 1 (mult 2 3)")
      , eq ":step infix input" "1 + 2 * 3\n    ^~~~~\n~mult~> 1 + 6" (outT ":step 1 + 2 * 3")
      , eq ":follow" (unlinesNoEnd
          [ "if (1 < 2) 10 20"
          , "   ^~~~~~~"
          , "~lt~> if true 10 20"
          , "      ^~~~~~~~~~~~~"
          , "~if~> 10"
          ]) (outT ":follow if (lt 1 2) 10 20")
      , eq ":type primitive" "plus : Int -> Int -> Int" (outT ":type plus")
      , eq ":type if" "if : Bool -> α -> α -> α" (outT ":type if")
      , eq "definition may use primitives" "d := \\n. n + n" (render rD)
      , eq ":type of definition" "d : Int -> Int" (render (runInput ctxD ":type d"))
      ]
  ]
  where
    ctx = pureCtx lectureEnv
    render = renderResult plainPalette
    out c = render (runInput ctx c)
    errOf c = case runInput ctx c of
      CommandErr e -> e
      r -> "not an error: " ++ show r
    typed = Ctx [] True
    outT c = render (runInput typed c)
    (ctx1, r1) = runLine ctx "three := suc 2"
    (ctx2, _) = runLine ctx "K := \\a b. b"
    (ctxH, rH) = runLine ctx "h := ..."
    (ctxH2, _) = runLine ctxH "h2 := h I"
    outH c = render (runInput ctxH2 c)
    (ctxD, rD) = runLine typed "d := \\n. plus n n"

------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------

-- | Random pure terms: bound variables x, y, z; free variables a, b;
-- the names K and I of the lecture environment.
newtype Term = Term Expr

instance Show Term where
  show (Term e) = prettyExpr e

instance QC.Arbitrary Term where
  arbitrary = Term <$> QC.sized (gen [])
    where
      gen bound n = QC.frequency $
        [ (3, Var <$> QC.elements (bound ++ ["a", "b", "K", "I"])) ]
        ++ [ (2, do x <- QC.elements ["x", "y", "z"]
                    Lam x Nothing <$> gen (x : bound) (n `div` 2)) | n > 0 ]
        ++ [ (2, App <$> gen bound (n `div` 2) <*> gen bound (n `div` 2)) | n > 0 ]
  shrink (Term e) = map Term (case e of
    App f a -> [f, a]
    Lam _ _ b -> [b]
    _ -> [])

newtype SimpleType = SimpleType Type

instance Show SimpleType where
  show (SimpleType t) = prettyType t

instance QC.Arbitrary SimpleType where
  arbitrary = SimpleType <$> QC.sized gen
    where
      gen n = QC.frequency $
        [ (2, TVar <$> QC.elements ["a", "b", "c"]), (1, TCon <$> QC.elements ["Int", "Bool"]) ]
        ++ [ (2, TArr <$> gen (n `div` 2) <*> gen (n `div` 2)) | n > 0 ]

propertyTests :: Test
propertyTests = group "properties"
  [ prop "pretty-printing round-trips through the parser" $ \(Term e) ->
      parseExpr (prettyExpr e) == Right e
  , prop "α-equivalence is reflexive" $ \(Term e) -> alphaEq e e
  , prop "α-equivalence is symmetric" $ \(Term e) (Term f) -> alphaEq e f == alphaEq f e
  , prop "renaming a binder keeps α-equivalence" $ \(Term e) ->
      alphaEq e (rename' e)
  , prop "substituting a variable for itself changes nothing" $ \(Term e) ->
      alphaEq (subst "x" (Var "x") e) e
  , prop "substitution only introduces free variables of the replacement" $ \(Term e) (Term n) ->
      null (freeVars (subst "x" n e) \\ ((freeVars e \\ ["x"]) ++ freeVars n))
  , prop "expanding names leaves no environment names free" $ \(Term e) ->
      null (freeVars (expandAll lectureEnv e) `intersectNames` map fst lectureEnv)
  , prop "a normal form has no redex" $ \(Term e) ->
      case normalFormWith Lazy ctx 200 e of
        Left TooManySteps -> True
        Right nf -> isNf nf && case stepBeta Lazy ctx nf of NoRedex _ -> True; _ -> False
  , prop "a normal form is β-equal to its term" $ \(Term e) ->
      case normalFormWith Lazy ctx 200 e of
        Left TooManySteps -> True
        Right nf -> betaEq ctx 500 e nf == Equal
  , prop "call-by-need gives the normal-order normal form" $ \(Term e) ->
      case normalFormWith Lazy ctx 200 e of
        Left TooManySteps -> True
        Right nf -> fmap (alphaEq nf) (normalizeNeed (ctxEnv ctx) 100000 e) == Just True
  , prop "an η-contraction is one η-step away" $ \(Term e) ->
      all (\c -> alphaEq (etaReduce c) (etaReduce e)) (etaContractions e)
  , prop "η-reduction is idempotent" $ \(Term e) -> etaReduce (etaReduce e) == etaReduce e
  , prop "erasing annotations is idempotent" $ \(Term e) -> erase (erase e) == erase e
  , prop "Church numerals decode to themselves" $ QC.forAll (QC.choose (0, 20 :: Integer)) $ \n ->
      prettyDecoded (churchNumeral n) == "⌜" ++ show n ++ "⌝"
  , prop "plus on Church numerals adds" $ QC.forAll small $ \(n, m) ->
      decodeNf (apps (Var "plus") [churchNumeral n, churchNumeral m]) == "⌜" ++ show (n + m) ++ "⌝"
  , prop "mult on Church numerals multiplies" $ QC.forAll small $ \(n, m) ->
      decodeNf (apps (Var "mult") [churchNumeral n, churchNumeral m]) == "⌜" ++ show (n * m) ++ "⌝"
  , prop "type equality up to renaming is reflexive" $ \(SimpleType t) -> typesEqualUpToRenaming t t
  , prop "a type is an instance of itself" $ \(SimpleType t) -> isInstanceOf t t
  , prop "printing a type round-trips through the parser" $ \(SimpleType t) ->
      parseType (prettyType t) == Right t
  ]
  where
    ctx = pureCtx lectureEnv
    small = (,) <$> QC.choose (0, 6) <*> QC.choose (0, 6 :: Integer)
    decodeNf e = either (const "diverged") prettyDecoded (normalFormWith Lazy ctx 5000 e)
    intersectNames xs ys = [ x | x <- xs, x `elem` ys ]
    -- Rename every binder x to x' (fresh by construction: primes are never generated).
    rename' e = case e of
      Lam x t b -> Lam (x ++ "'") t (rename' (subst x (Var (x ++ "'")) b))
      App f a -> App (rename' f) (rename' a)
      other -> other

------------------------------------------------------------------------
-- Files: the happy paths
------------------------------------------------------------------------

fileTests :: IO Test
fileTests = do
  okPath <- dataFile "test/data/ok.lam"
  ok' <- checkFile okPath
  loaded <- loadFile okPath
  sess <- loadSession okPath
  typed <- dataFile "test/data/typed.lam" >>= checkFile
  typedLd <- dataFile "test/data/typed.lam" >>= loadFile
  prelude <- dataFile "test/data/prelude.lam" >>= checkFile
  bad <- dataFile "test/data/bad.lam" >>= checkFile
  missing <- checkFile "test/data/does-not-exist.lam"
  cyc <- dataFile "test/data/cyc-a.lam" >>= checkFile
  mismatch <- dataFile "test/data/lang-mismatch.lam" >>= checkFile
  impMissing <- dataFile "test/data/imp-missing.lam" >>= checkFile
  impExt <- dataFile "test/data/imp-ext.lam" >>= checkFile
  return $ group "files"
    [ group "ok.lam" $ case ok' of
        Left err -> [ok ("loads: " ++ err) False]
        Right tasks ->
          [ eq "tasks" 12 (length tasks)
          , TestList [ eq (trId t ++ " " ++ show (trChecks t)) Done (taskStatus t) | t <- tasks ]
          , ok "reportOk" (reportOk tasks)
          , ok "prettyResults" ("task 1.1 (скобки): DONE" `isInfixOf` prettyResults tasks)
          ]
    , group "ok.lam loadFile" $ case loaded of
        Left err -> [ok ("loads: " ++ err) False]
        Right ld ->
          [ eq "vars" ["x", "y", "z", "f", "g"] (ldVars ld)
          , eq "language" Pure (ldLanguage ld)
          , eq "problems" [] (ldProblems ld)
          , eq "imports come first" (Just "I") (fmap fst (safeHead (ldEnv ld)))
          , ok "own definitions after imports" ("or" `elem` map fst (ldEnv ld))
          ]
    , group "ok.lam loadSession" $ case sess of
        Left err -> [ok ("loads: " ++ err) False]
        Right s ->
          [ eq "file" okPath (sessionFile s)
          , eq "pure" False (ctxTyped (sessionCtx s))
          , eq "env" (Just (pure' "\\x y. x")) (lookup "K" (ctxEnv (sessionCtx s)))
          ]
    , group "typed.lam" $ case typed of
        Left err -> [ok ("loads: " ++ err) False]
        Right tasks -> [ eq (trId t ++ " " ++ show (trChecks t)) Done (taskStatus t) | t <- tasks ]
    , eq "typed.lam language" (Right Typed) (fmap ldLanguage typedLd)
    , eq "file without tasks" (Right 0) (fmap length prelude)
    , group "bad.lam" $ case bad of
        Left err -> [ok ("loads: " ++ err) False]
        Right tasks ->
          let task tid = maybe (error tid) id (lookup tid [ (trId t, t) | t <- tasks ])
              status = taskStatus . task
              messages tid = [ m | CheckResult _ (Left m) <- trChecks (task tid) ]
              says tid s = ok (tid ++ " says " ++ s) (any (s `isInfixOf`) (messages tid))
              report = prettyResults tasks
          in
          [ eq "1.1 hole" Failed (status "1.1")
          , eq "1.2 partial" (Partial 1 2) (status "1.2")
          , eq "1.3 wrong step" (Partial 1 3) (status "1.3")
          , eq "1.4 parens" Failed (status "1.4")
          , says "1.4" "остались лишние скобки: можно убрать ещё 1 пару скобок"
          , ok "1.4 minimal never prints the answer" (not (any ("x (y x)" `isInfixOf`) (messages "1.4")))
          , eq "1.5 needs delta" (Partial 1 2) (status "1.5")
          , eq "1.6 typo" Failed (status "1.6")
          , eq "1.7 omega" Failed (status "1.7")
          , eq "1.8 wrong name" (Partial 3 4) (status "1.8")
          , eq "1.9 legacy ~d~>" (Partial 3 4) (status "1.9")
          , eq "1.10 arrow name" Failed (status "1.10")
          , ok "hole messages end with (...)" (all isHoleMessage (messages "1.1"))
          , ok "wrong answers are not holes" (not (any isHoleMessage (messages "1.2")))
          , says "1.2" "слева получается"
          , says "1.8" "раскрытием ‘K’, а не ‘S’"
          , says "1.9" "раскрытие имени пишется с самим именем"
          , says "1.10" "занято стрелкой ~s~>"
          , says "1.5" "сначала раскройте его отдельной строкой"
          , ok "not reportOk" (not (reportOk tasks))
          , ok "prettyResults status" ("task 1.2 (неверный ответ, PARTIAL): PARTIAL 1/2" `isInfixOf` report)
          , ok "prettyResults message indent" ("\n    expect t x = x\n      в ‘t’ осталась дырка (...)" `isInfixOf` report)
          ]
    , eq "isHoleMessage" [True, False] (map isHoleMessage ["в ‘t’ осталась дырка (...)", "слева получается ⌜7⌝, а справа ⌜8⌝"])
    , ok "missing file" (leftHas "не удаётся открыть" missing)
    , group "imports"
        [ ok "cyclic import" (leftHas "циклический import" cyc)
        , ok "language mismatch" (leftHas "написан на другом language" mismatch)
        , ok "missing file" (leftHas "не удаётся открыть" impMissing)
        , eq "with extension" (Right True) (fmap reportOk impExt)
        ]
    ]
  where
    safeHead (x : _) = Just x
    safeHead [] = Nothing

------------------------------------------------------------------------
-- Files: deliberate mistakes and the messages a student reads
------------------------------------------------------------------------

-- Exercise the whole .lam checking path: constants must be available only
-- in language typed, including when used as arguments to bound functions.
inhabitantFileTests :: IO Test
inhabitantFileTests = do
  typed <- dataFile "test/data/inhabit-typed.lam" >>= checkFile
  pureFile <- dataFile "test/data/inhabit-pure.lam" >>= checkFile
  return $ group "inhabitants and literals"
    [ statuses "typed" typed
        [ ("1.1", Failed), ("1.2", Failed), ("1.3", Failed), ("1.4", Failed)
        , ("1.5", Done), ("1.6", Done)
        , ("2.1", Failed), ("2.2", Failed), ("2.3", Failed), ("2.4", Failed)
        , ("2.5", Done), ("2.6", Done)
        , ("3.1", Done), ("3.2", Done), ("3.3", Done), ("3.4", Done)
        , ("3.5", Partial 1 2), ("3.6", Partial 4 5)
        , ("3.7", Done), ("3.8", Failed), ("3.9", Failed)
        ]
    , statuses "pure" pureFile
        [ ("1.1", Done), ("1.2", Done), ("1.3", Done), ("1.4", Done)
        , ("1.5", Done), ("1.6", Done), ("1.7", Done), ("1.8", Failed)
        ]
    ]
  where
    statuses label loaded expected = group label $ case loaded of
      Left err -> [ok ("loads: " ++ err) False]
      Right tasks ->
        [ eq tid (Just want) (lookup tid [(trId t, taskStatus t) | t <- tasks])
        | (tid, want) <- expected
        ]

errorFileTests :: IO Test
errorFileTests = do
  errs <- dataFile "test/data/errors.lam" >>= checkFile
  errsLd <- dataFile "test/data/errors.lam" >>= loadFile
  typedBad <- dataFile "test/data/typed-bad.lam" >>= checkFile
  return $ group "error files"
    [ group "errors.lam" $ withTasks errs $ \tasks task status says ->
        [ eq "task count" 22 (length tasks)
        , eq "2.1 duplicate" Failed (status "2.1")
        , eq "2.1 duplicate reported once per definition" 2 (length (trChecks (task "2.1")))
        , says "2.1" "повторное определение ‘dup’"
        , eq "2.2 self reference" Failed (status "2.2")
        , says "2.2" "‘loop’ ссылается на себя"
        , eq "2.3 undefined" Failed (status "2.3")
        , says "2.3" "в ‘bad’ не определено: missing"
        , eq "2.4 subst outside chain" Failed (status "2.4")
        , says "2.4" "подстановка [x |-> N] M допустима только первой строкой chain"
        , eq "2.5 ~s~> without subst" (Partial 1 2) (status "2.5")
        , says "2.5" "~s~> допустима только после строки вида [x |-> N] M"
        , eq "2.6 ~~> with strategy" (Partial 1 2) (status "2.6")
        , says "2.6" "~~> нельзя использовать в цепочке со стратегией"
        , eq "2.7 from mismatch" (Partial 4 5) (status "2.7")
        , says "2.7" "первая строка должна быть K x y"
        , eq "2.8 to mismatch" (Partial 2 3) (status "2.8")
        , says "2.8" "последняя строка должна быть эквивалентна \\x. x, а это \\y. x"
        , eq "2.9 unfinished chain" (Partial 1 2) (status "2.9")
        , says "2.9" "в последнем терме остались редексы после раскрытия имён: (\\x y. x) x y"
        , eq "2.10 wrong strategy step" (Partial 1 2) (status "2.10")
        , says "2.10" "это не шаг стратегии applicative; ожидалось: (\\x. y) (\\x. x)"
        , eq "2.11 no redex" (Partial 1 2) (status "2.11")
        , says "2.11" "в строке 1 нет редексов"
        , eq "2.12 not alpha" (Partial 1 2) (status "2.12")
        , says "2.12" "строки 1 и 2 не α-эквивалентны"
        , eq "2.13 too few steps" (Partial 1 2) (status "2.13")
        , says "2.13" "строка 2 не достигается из строки 1 за 1 шагов"
        , eq "2.14 undecided expect" Failed (status "2.14")
        , says "2.14" "не удалось сравнить за лимит шагов"
        , eq "2.15 free" Failed (status "2.15")
        , says "2.15" "свободные переменные: y, а не x"
        , eq "2.16 rename" Failed (status "2.16")
        , says "2.16" "связыватели должны быть попарно различны"
        , says "2.16" "терм не α-эквивалентен ‘rn’: \\x x. x"
        , eq "2.17 minimal" Failed (status "2.17")
        , says "2.17" "имена связанных переменных менять не нужно"
        , says "2.17" "это другой терм, не ‘mn’"
        , ok "2.17 minimal never prints the answer"
            (not (any ("x (y x)" `isInfixOf`) [ m | CheckResult _ (Left m) <- trChecks (task "2.17") ]))
        , eq "2.18 predicates" (Partial 2 8) (status "2.18")
        , says "2.18" "терм не в нормальной форме: \\x. (\\y. y) x"
        , says "2.18" "предикат ‘whnf’ выполняется, а не должен"
        , says "2.18" "определение не использует ‘K’"
        , says "2.18" "терм не в HNF"
        , says "2.18" "терм не замкнут, свободны: y"
        , says "2.18" "предикат ‘closed’ выполняется, а не должен"
        , ok "2.18 check names carry the predicate" (any (("→ uses K" `isInfixOf`) . crName) (trChecks (task "2.18")))
        , eq "2.19 hole in chain" Failed (status "2.19")
        , ok "2.19 hole message" (all isHoleMessage [ m | CheckResult _ (Left m) <- trChecks (task "2.19") ])
        , eq "2.20 unknown name in expect" Failed (status "2.20")
        , says "2.20" "не определено: nope"
        , eq "2.21 empty section" Done (status "2.21")
        , eq "2.22 answer is not a normal form" (Partial 1 12) (status "2.22")
        , says "2.22" "справа не нормальная форма: ‘K 5 (Y (K 5))’ ещё можно вычислить"
        , says "2.22" "‘succ (succ 1)’ ещё можно вычислить"
        , says "2.22" "‘succ 1 succ 1’ ещё можно вычислить"
        , says "2.22" "‘(\\x y. x) y’ ещё можно вычислить"
        , says "2.22" "‘and (\\t e. t) (\\t e. t)’ ещё можно вычислить"
        , says "2.22" "справа не нормальная форма: ‘2 * 2’ ещё можно вычислить"
        , says "2.22" "справа не нормальная форма: ‘I x’ ещё можно вычислить"
        , says "2.22" "‘pair 1 2 K’ ещё можно вычислить"
        , says "2.22" "справа не нормальная форма: ‘I 2’ ещё можно вычислить"
        , says "2.22" "слева получается ⌜3⌝, а справа ⌜4⌝"
        , eq "2.22 a wrong form is not a hole, a missing answer is" [True]
            (filter id [ isHoleMessage m | CheckResult _ (Left m) <- trChecks (task "2.22") ])
        ]
    , group "errors.lam loadFile" $ case errsLd of
        Left err -> [ok ("loads: " ++ err) False]
        Right ld ->
          [ eq "problem names" ["dup", "loop", "bad", "sub"] (map fst (ldProblems ld))
          , ok "duplicate keeps the first definition" (lookup "dup" (ldEnv ld) == Just (pure' "\\x. x"))
          ]
    , group "typed-bad.lam" $ withTasks typedBad $ \_ _ status says ->
        [ eq "3.1 type" (Partial 1 6) (status "3.1")
        , says "3.1" "тип a -> a -> a верен, но не наиболее общий; наиболее общий: α -> β -> α"
        , says "3.1" "наиболее общий тип: α -> β -> α, а не a -> a"
        , says "3.1" "терм типизируется: α -> β -> α"
        , says "3.1" "ответ не дан (...)"
        , says "3.1" "терм не типизируется: бесконечный тип: α ∼ α -> β"
        , eq "3.2 church" (Partial 1 5) (status "3.2")
        , says "3.2" "терм не типизируется по аннотациям: не удаётся унифицировать a и a -> a"
        , says "3.2" "аннотации дают тип a -> a -> a, а наиболее общий: α -> β -> β"
        , says "3.2" "после стирания аннотаций получается K I, а нужно (\\x y. x) (\\x. x)"
        , says "3.2" "в терме осталась дырка (...)"
        , eq "3.3 inhabit" (Partial 8 14) (status "3.3")
        , says "3.3" "тип терма α -> α не обобщает a -> a -> a"
        , says "3.3" "тип a -> a населён, обитатель есть"
        , says "3.3" "‘k1’ и ‘k2’ αβη-эквивалентны"
        , says "3.3" "терм не замкнут, свободны: y"
        , says "3.3" "F ⌜0⌝ и F ⌜1⌝ αβη-эквивалентны"
        , says "3.3" "ответ не дан (...)"
        , eq "3.4 constants" Failed (status "3.4")
        , says "3.4" "слева получается plus 1 true, а справа 2"
        , says "3.4" "наиболее общий тип: Int -> Int, а не Int -> Int -> Int"
        , eq "3.5 answer is not a normal form" (Partial 1 3) (status "3.5")
        , says "3.5" "справа не нормальная форма: ‘21 + 21’ ещё можно вычислить"
        , says "3.5" "справа не нормальная форма: ‘2 * 3’ ещё можно вычислить"
        ]
    ]
  where
    -- Hand the tasks, a lookup by id, its status and a message check to the
    -- assertions, or report the load error as the only test of the group.
    withTasks loaded body = case loaded of
      Left err -> [ok ("loads: " ++ err) False]
      Right tasks ->
        let task tid = maybe (error ("no task " ++ tid)) id (lookup tid [ (trId t, t) | t <- tasks ])
            status = taskStatus . task
            messages tid = [ m | CheckResult _ (Left m) <- trChecks (task tid) ]
            says tid s = ok (tid ++ " says " ++ s) (any (s `isInfixOf`) (messages tid))
        in  body tasks task status says
