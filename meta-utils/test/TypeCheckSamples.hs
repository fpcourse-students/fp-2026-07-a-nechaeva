{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- Выражения-эталоны записаны так, как их видел бы студент в условии.
{- HLINT ignore "Redundant lambda" -}
{- HLINT ignore "Use tuple-section" -}
{- HLINT ignore "Use (,)" -}

-- | Образцы для тестов "TypeCheck": то, что в домашке лежало бы в модуле студента.
-- Отдельный модуль нужен Template Haskell: reify видит только уже скомпилированные имена.
--
-- Образцы намеренно не совпадают с ответами домашек: этот файл лежит и в репозитории студента.
module TypeCheckSamples where

import Data.Kind (Type)
import TypeCheck (Todo)

-- Синонимы: один и тот же тип в разных записях и его искажения.

type Sample a b = forall c. (a -> c) -> b -> (c, b)
type SampleRenamed x y = forall r. (x -> r) -> y -> (r, y)
type SampleParens a b = forall c. ((a -> c) -> (b -> (c, b)))
type SampleKinded a b = forall (c :: Type). (a -> c) -> b -> (c, b)
type SampleSwappedArgs a b = forall c. b -> (a -> c) -> (c, b)
type SampleSwappedParams b a = forall c. (a -> c) -> b -> (c, b)
type SampleExtraParam a b c = (a -> c) -> b -> (c, b)
type SampleOther a b = (a, b)

type NoParams = forall a. (a -> a -> a) -> a

type Stub = Todo
type StubWithParams a b = Todo

type Name = String
type NameAsList = [Char]
type Table a = [(Int, Maybe a)]

data NotSynonym = NotSynonym

-- Значения с сигнатурами: проверяется только запись типа, поэтому тела — заглушки.

keepFirst :: a -> b -> a
keepFirst = undefined

keepFirstRenamed :: x -> y -> x
keepFirstRenamed = undefined

keepFirstExplicit :: forall a b. a -> b -> a
keepFirstExplicit = undefined

keepFirstReordered :: forall b a. a -> b -> a
keepFirstReordered = undefined

keepSecond :: a -> b -> b
keepSecond = undefined

tooSpecific :: Int -> Bool -> Int
tooSpecific = undefined

withContext :: (Show a, Eq a) => a -> String
withContext = undefined

withContextReordered :: (Eq a, Show a) => a -> String
withContextReordered = undefined

withoutContext :: a -> String
withoutContext = undefined

rankTwo :: (forall a. a -> a) -> (Int, Bool)
rankTwo _ = undefined

rankOne :: forall a. (a -> a) -> (Int, Bool)
rankOne = undefined

stubValue :: Todo
stubValue = undefined

-- Значения без сигнатур: их тип выводит GHC. Так в тесте записывается эталон задачи
-- «предскажите тип выражения» — выражением, а не ответом.

inferredTwice = \f x -> f (f x)
inferredSwap = \(x, y) -> (y, x)
inferredApplyTo = \x f -> f x
inferredPairWith = \x y -> (x, y)
inferredPairWithFlipped = flip (\y x -> (x, y))

predictedTwice :: (a -> a) -> a -> a
predictedTwice = undefined

predictedSwap :: (p, q) -> (q, p)
predictedSwap = undefined

predictedApplyTo :: t -> (t -> r) -> r
predictedApplyTo = undefined

predictedApplyToWrong :: t -> (r -> t) -> r
predictedApplyToWrong = undefined

predictedTwiceTooGeneral :: (a -> b) -> a -> b
predictedTwiceTooGeneral = undefined

predictedTwiceTooSpecific :: (Int -> Int) -> Int -> Int
predictedTwiceTooSpecific = undefined
