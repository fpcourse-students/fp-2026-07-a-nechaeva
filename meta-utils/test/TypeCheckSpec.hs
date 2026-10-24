-- | Тесты "TypeCheck": что форма типа считает одинаковым, что различает, как ведут себя
-- заглушка и неверный ответ.
module TypeCheckSpec (tests) where

import Control.Exception (try)
import Data.List (isInfixOf)
import Test.HUnit (Test (..), assertBool, assertEqual, assertFailure)
import Test.HUnit.Lang (HUnitFailure (..), formatFailureReason)
import TodoException (TodoException)
import TypeCheck
import TypeCheckSamples

tests :: Test
tests = TestList
  [ TestLabel "синонимы: что считается одинаковым" testSynonymSame
  , TestLabel "синонимы: что различается" testSynonymDifferent
  , TestLabel "синонимы: точный вид формы" testSynonymExact
  , TestLabel "сигнатуры" testSignatures
  , TestLabel "предсказание типа: эталон выводит GHC" testPrediction
  , TestLabel "заглушка" testStub
  , TestLabel "исход проверки" testOutcome
  , TestLabel "typeIs" testTypeIs
  ]

same, different :: String -> String -> String -> Test
same what l r = TestCase $ assertEqual what l r
different what l r = TestCase $ assertBool (what <> ": формы совпали: " <> l) $ l /= r

sample :: String
sample = $(synonymShape ''Sample)

testSynonymSame :: Test
testSynonymSame = TestList
  [ same "имена переменных" sample $(synonymShape ''SampleRenamed)
  , same "лишние скобки" sample $(synonymShape ''SampleParens)
  , same "аннотация кайнда Type" sample $(synonymShape ''SampleKinded)
  , same "String и [Char]" $(synonymShape ''Name) $(synonymShape ''NameAsList)
  ]

testSynonymDifferent :: Test
testSynonymDifferent = TestList
  [ different "порядок аргументов функции" sample $(synonymShape ''SampleSwappedArgs)
  , different "порядок параметров синонима" sample $(synonymShape ''SampleSwappedParams)
  , different "лишний параметр вместо квантора" sample $(synonymShape ''SampleExtraParam)
  , different "другой тип" sample $(synonymShape ''SampleOther)
  , different "другой синоним с квантором" sample $(synonymShape ''NoParams)
  ]

testSynonymExact :: Test
testSynonymExact = TestList
  [ same "параметры и квантор" "2 (forall v3 (-> (-> v1 v3) (-> v2 ((,) v3 v2))))" sample
  , same "без параметров" "0 (forall v1 (-> (-> v1 (-> v1 v1)) v1))" $(synonymShape ''NoParams)
  , same "списки, кортежи, конструкторы" "1 ([] ((,) Int (Maybe v1)))" $(synonymShape ''Table)
  , same "не синоним" "<not a type synonym>" $(synonymShape ''NotSynonym)
  ]

keepFirstShape :: String
keepFirstShape = $(shapeOfType 'keepFirst)

testSignatures :: Test
testSignatures = TestList
  [ same "точный вид" "(forall v1 v2 (-> v1 (-> v2 v1)))" keepFirstShape
  , same "имена переменных" keepFirstShape $(shapeOfType 'keepFirstRenamed)
  , same "явный forall" keepFirstShape $(shapeOfType 'keepFirstExplicit)
  , same "порядок переменных под forall" keepFirstShape $(shapeOfType 'keepFirstReordered)
  , different "какой аргумент возвращается" keepFirstShape $(shapeOfType 'keepSecond)
  , different "частный случай общего типа" keepFirstShape $(shapeOfType 'tooSpecific)
  , same "порядок ограничений" $(shapeOfType 'withContext) $(shapeOfType 'withContextReordered)
  , different "наличие ограничений" $(shapeOfType 'withContext) $(shapeOfType 'withoutContext)
  , different "вложенность квантора" $(shapeOfType 'rankTwo) $(shapeOfType 'rankOne)
  ]

-- | Эталон — выражение без сигнатуры, его тип выводит GHC; ответ студента — сигнатура
-- при заглушке. Формы сравниваются, сам ответ в тесте не записан.
testPrediction :: Test
testPrediction = TestList
  [ same "верное предсказание" $(shapeOfType 'inferredTwice) $(shapeOfType 'predictedTwice)
  , same "свои имена переменных" $(shapeOfType 'inferredSwap) $(shapeOfType 'predictedSwap)
  , same "функция-аргумент" $(shapeOfType 'inferredApplyTo) $(shapeOfType 'predictedApplyTo)
  , different "неверное предсказание" $(shapeOfType 'inferredApplyTo) $(shapeOfType 'predictedApplyToWrong)
  , different "слишком общий тип" $(shapeOfType 'inferredTwice) $(shapeOfType 'predictedTwiceTooGeneral)
  , different "слишком частный тип" $(shapeOfType 'inferredTwice) $(shapeOfType 'predictedTwiceTooSpecific)
  , same "два выражения одного типа" $(shapeOfType 'inferredPairWith) $(shapeOfType 'inferredPairWithFlipped)
  ]

testStub :: Test
testStub = TestList
  [ TestCase $ assertBool "синоним-заглушка" $ isTodo $(synonymShape ''Stub)
  , TestCase $ assertBool "синоним-заглушка с параметрами" $ isTodo $(synonymShape ''StubWithParams)
  , TestCase $ assertBool "сигнатура-заглушка" $ isTodo $(shapeOfType 'stubValue)
  , TestCase $ assertBool "ответ — не заглушка" $ not $ isTodo sample
  , TestCase $ assertBool "тип с Todo внутри — не заглушка" $ not $ isTodo "1 (Maybe Todo)"
  ]

data Outcome = Passed | Failed String | NotStarted deriving (Eq, Show)

outcome :: Test -> IO Outcome
outcome = \case
  TestCase action -> try (try action) >>= \case
    Left (_ :: TodoException) -> pure NotStarted
    Right (Left (HUnitFailure _ reason)) -> pure $ Failed $ formatFailureReason reason
    Right (Right ()) -> pure Passed
  _ -> assertFailure "ожидался TestCase" >> pure Passed

testOutcome :: Test
testOutcome = TestList
  [ TestCase $ outcome (assertShape "задача" sample $(synonymShape ''SampleRenamed)) >>=
      assertEqual "верный ответ" Passed
  , TestCase $ outcome (assertShape "задача" sample $(synonymShape ''Stub)) >>=
      assertEqual "заглушка — не провал, а «не начато»" NotStarted
  , TestCase $ outcome (assertShape "задача" sample $(synonymShape ''SampleOther)) >>= \case
      Failed message -> do
        assertBool "в сообщении есть имя задачи" $ "задача" `isInfixOf` message
        assertBool "в сообщении нет ожидаемого ответа" $ not $ sample `isInfixOf` message
      other -> assertFailure $ "неверный ответ должен быть провалом, а получено " <> show other
  , TestCase $ outcome (assertShapeMarked "задача" "Todo" sample sample) >>=
      assertEqual "маркер-заглушка важнее совпадения формы" NotStarted
  ]

testTypeIs :: Test
testTypeIs = TestList
  [ TestCase $ outcome (typeIs @(Maybe Int) @(Maybe Int) "задача") >>= assertEqual "тот же тип" Passed
  , TestCase $ outcome (typeIs @Name @NameAsList "задача") >>= assertEqual "синонимы раскрываются" Passed
  , TestCase $ outcome (typeIs @Int @Todo "задача") >>= assertEqual "заглушка" NotStarted
  , TestCase $ outcome (typeIs @Int @Bool "задача") >>= \case
      Failed message -> assertBool "в сообщении нет ожидаемого ответа" $ not $ "Int" `isInfixOf` message
      other -> assertFailure $ "неверный тип должен быть провалом, а получено " <> show other
  ]
