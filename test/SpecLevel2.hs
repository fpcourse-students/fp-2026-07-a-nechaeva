module SpecLevel2 where

import Data.Foldable (toList)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Data.Ord (Down (..))
import Level2
import Test.Prelude

tests :: NamedTests
tests = nameTests 2 [testSpecial, testUnfold, testMap, testUnion, testFold, testFormat, testRights]

fromNat :: Nat -> Int
fromNat Z = 0
fromNat (S n) = 1 + fromNat n

toNat :: Int -> Nat
toNat n = iterate S Z !! n

testSpecial :: Test
testSpecial = TestList
  [ propertyToTest "сумма Nat" $ forAll (chooseInt (0,30)) $ \n ->
      forAll (chooseInt (0,30)) $ \m ->
        fromNat (sumOrLength (Left (toNat n, toNat m))) === n + m
  , propertyToTest "длина строки" $ \s -> fromNat (sumOrLength (Right s)) === length s
  , TestCase $ assertEqual "нули" Z (sumOrLength (Left (Z,Z))) ]

testUnfold :: Test
testUnfold = TestList
  [ propertyToTest "как map" $ \(Fun _ f :: Fun Int Char) xs -> map' f xs === map f xs
  , TestCase $ assertEqual "конечный префикс" [2,3,4 :: Int] $
      take 3 (map' (+1) ([1..3] ++ infinite))
  , TestCase $ assertEqual "не форсирует значение элемента" [7 :: Int] $
      take 1 (map' (const 7) [infinite]) ]

testMap :: Test
testMap = TestList
  [ TestCase $ assertEqual "пустой" (Nothing :: Maybe Int) (myLookup myEmpty False)
  , TestCase $ do
      let initial = MyMap (\k -> if k then Just (9 :: Int) else Nothing)
          inserted = myInsert False 7 initial
      assertEqual "вставка" (Just 7) (myLookup inserted False)
      assertEqual "сохраняется другой ключ" (Just 9) (myLookup inserted True)
      assertEqual "замена" (Just 8) (myLookup (myInsert False 8 inserted) False)
  , propertyToTest "массовая вставка, последний дубликат" $ \(kvs :: [(Bool, Int)]) ->
      let initial = MyMap (const (Just 100))
          expected = Map.union (Map.fromList kvs) (Map.fromList [(False,100),(True,100)])
          actual = myInsertMany initial kvs
      in map (myLookup actual) [False,True] === map (`Map.lookup` expected) [False,True]
  , TestCase $ do
      let initial = MyMap (const (Just (8 :: Int)))
      assertEqual "удаление" Nothing (myLookup (myDelete False initial) False)
      assertEqual "другой ключ" (Just 8) (myLookup (myDelete False initial) True)
      assertEqual "удаление отсутствующего" (Nothing :: Maybe Int) (myLookup (myDelete False myEmpty) False)
      assertEqual "массовое удаление" [Nothing,Nothing] $
        map (myLookup (myDeleteMany initial [False,True,False])) [False,True]
      assertEqual "Foldable Maybe" (Just 3) $
        myLookup (myInsertMany initial (Just (False,3))) False ]

-- Словари инстансов проверяются на независимых готовых MyMap,
-- чтобы незавершённая 2.3 не блокировала задачи 2.4 и 2.5.
asMyMap :: Map.Map Bool Int -> MyMap Bool Int
asMyMap m = MyMap (`Map.lookup` m)

observe :: MyMap Bool Int -> [Maybe Int]
observe m = map (myLookup m) [False,True]

testUnion :: Test
testUnion = TestList
  [ propertyToTest "левостороннее объединение" $ \(a :: Map.Map Bool Int) b ->
      observe (asMyMap a <> asMyMap b) === observe (asMyMap (Map.union a b))
  , propertyToTest "Associativity" $ \(a :: Map.Map Bool Int) b c ->
      let x = asMyMap a; y = asMyMap b; z = asMyMap c
      in observe ((x <> y) <> z) === observe (x <> (y <> z))
  , propertyToTest "Left Identity" $ \(a :: Map.Map Bool Int) ->
      observe (mempty <> asMyMap a) === observe (asMyMap a)
  , propertyToTest "Right Identity" $ \(a :: Map.Map Bool Int) ->
      observe (asMyMap a <> mempty) === observe (asMyMap a)
  , TestCase $ assertEqual "значения не требуют Semigroup" (Just (4 :: Int)) $
      myLookup (MyMap (const (Just 4)) <> MyMap (const (Just 5))) False ]

testFold :: Test
testFold = TestList
  [ propertyToTest "порядок ключей и пропуск Nothing" $ \(m :: Map.Map Bool Int) ->
      toList (asMyMap m) === Map.elems m
  , propertyToTest "Sum" $ \(m :: Map.Map Bool Int) ->
      getSum (foldMap Sum (asMyMap m)) === sum (Map.elems m)
  , TestCase $ assertEqual "равные значения — разные ключи" [7,7 :: Int] $
      toList (MyMap (const (Just 7)) :: MyMap Bool Int)
  , TestCase $ assertEqual "пустой" ([] :: [Int]) $
      toList (MyMap (const Nothing) :: MyMap Bool Int) ]

referenceFormat :: WordCountShow -> Map.Map String Int -> String
referenceFormat cfg = unlines . map (\(w,n) -> w ++ " : " ++ show n)
  . take (topSize cfg) . List.sortOn (\(w,n) -> (Down n,w))
  . filter (\(w,_) -> length w > smallWordsSize cfg) . Map.toList

testFormat :: Test
testFormat = TestList
  [ TestCase $ assertEqual "фильтрация до top, ничья, завершающий перевод строки"
      "aaaa : 3\nbbbb : 3\n" $
      wordCountShow (WordCountShow 2 3) (Map.fromList [("a",100),("bbbb",3),("aaaa",3),("cccc",1)])
  , TestCase $ assertEqual "граница длины" "abcd : 1\n" $
      wordCountShow (WordCountShow 10 3) (Map.fromList [("abc",2),("abcd",1)])
  , TestCase $ assertEqual "отрицательный top" "" $
      wordCountShow (WordCountShow (-1) 0) (Map.singleton "a" 1)
  , TestCase $ assertEqual "пустой ввод" "" (wordCountShow stdWcShow Map.empty)
  , propertyToTest "порядок, фильтр, настройки" $ \(m :: Map.Map String (NonNegative Int)) ->
      forAll (chooseInt (-2,10)) $ \top -> forAll (chooseInt (-1,5)) $ \small ->
        let cfg = WordCountShow top small; counts = fmap getNonNegative m
        in wordCountShow cfg counts === referenceFormat cfg counts ]

testRights :: Test
testRights = TestList
  [ propertyToTest "значения и порядок" $ \(xs :: [Either Bool Int]) ->
      collectRights xs === [x | Right x <- xs]
  , TestCase $ assertEqual "ленивый хвост" [2,1 :: Int] $
      take 2 (collectRights (Right 2 : Left True : Right 1 : infinite)) ]
