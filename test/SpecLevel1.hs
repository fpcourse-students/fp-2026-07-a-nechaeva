module SpecLevel1 where

import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Examples qualified as E
import Level1
import Test.Prelude

tests :: NamedTests
tests = nameTests 1 [testPredict, testCounterexample, testElem, testCount, testTree, testWords]

testPredict :: Test
testPredict = TestList
  [ TestCase $ assertEqual "правая свёртка" (foldr (-) 0 [1,2,3]) predictedRight
  , TestCase $ assertEqual "левая свёртка" (foldl (-) 0 [1,2,3]) predictedLeft
  , TestCase $ assertEqual "вклад элемента Maybe"
      (foldMap (\x -> [x,x]) (Just (2 :: Integer))) predictedFoldMap ]

testCounterexample :: Test
testCounterexample = TestCase $ do
  let (earned, expenses) = incomeCounterexample
  assertBool "неотрицательные заработок и расходы" (earned >= 0 && all (>=0) expenses)
  assertBool "это должен быть контрпример"
    (E.brokenIncome earned expenses /= earned - sum expenses)

testElem :: Test
testElem = TestList
  [ propertyToTest "как elem на конечных списках" $ \(x :: Int) xs -> elem' x xs === elem x xs
  , TestCase $ assertBool "найденный элемент не требует хвоста" (elem' (4 :: Int) ([1..4] ++ infinite))
  , TestCase $ assertEqual "пустой список" False (elem' (0 :: Int) [])
  , TestCase $ assertBool "совпадение в голове" (elem' 'x' ('x' : infinite)) ]

testCount :: Test
testCount = TestList
  [ propertyToTest "подсчёт в списке" $ \(Fun _ p :: Fun Int Bool) xs ->
      countMatching p xs === length (filter p xs)
  , TestCase $ do
      assertEqual "пустой контейнер" (0 :: Integer) (countMatching odd ([] :: [Int]))
      assertEqual "Maybe" (1 :: Int) (countMatching odd (Just (3 :: Int)))
      assertEqual "Map — только значения" (2 :: Int) $
        countMatching even (Map.fromList [('a',2),('b',3),('c',4 :: Int)]) ]

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized gen
    where
      gen 0 = oneof [pure Leaf, Node <$> arbitrary <*> pure []]
      gen n = frequency
        [ (1, pure Leaf)
        , (3, do
            k <- chooseInt (0,3)
            Node <$> arbitrary <*> vectorOf k (gen (n `div` 3))) ]

dfs :: Tree a -> [a]
dfs Leaf = []
dfs (Node x children) = x : concatMap dfs children

testTree :: Test
testTree = TestList
  [ TestCase $ assertEqual "DFS, не BFS" [1,2,4,3] $
      toList (Node (1 :: Int) [Node 2 [Node 4 []], Leaf, Node 3 []])
  , TestCase $ do
      assertEqual "пустое дерево" ([] :: [Int]) (toList Leaf)
      assertEqual "узел без детей" [7 :: Int] (toList (Node 7 []))
  , propertyToTest "порядок и кратность" $ \(t :: Tree Int) -> toList t === dfs t
  , propertyToTest "моноид Sum" $ \(t :: Tree Int) ->
      getSum (foldMap Sum t) === sum (dfs t)
  , propertyToTest "foldr и foldMap сохраняют один порядок" $ \(t :: Tree Int) ->
      foldr (:) [] t === foldMap (\x -> [x]) t
  , lawsToTest $ foldableLaws (Proxy @Tree) ]

referenceWords :: [String] -> Map.Map String Int
referenceWords = Map.fromListWith (+) . map (,1) . E.tokens

testWords :: Test
testWords = TestList
  [ TestCase $ assertEqual "нет слов" Map.empty (wordCount ["", ", : !", "\t"])
  , TestCase $ assertEqual "апостроф, регистр, несколько строк"
      (Map.fromList [("alice",2),("s",1)]) (wordCount ["Alice's", "ALICE!"])
  , TestCase $ assertEqual "Unicode, табуляция, цифры"
      (Map.fromList [("ёж",2),("42",1)]) (wordCount ["ЁЖ—ёж\t42"])
  , propertyToTest "токены и кратность" $ \(xs :: [String]) -> wordCount xs === referenceWords xs ]
