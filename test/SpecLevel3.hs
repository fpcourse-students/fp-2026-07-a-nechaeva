module SpecLevel3 where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Level3
import SpecLevel1 (referenceWords)
import Test.Prelude

tests :: NamedTests
tests = nameTests 3 [testBfs, testIndex, testFind]

instance Arbitrary a => Arbitrary (NeighborTree a) where
  arbitrary = sized gen
    where
      gen 0 = pure NLeaf
      gen n = frequency
        [(1,pure NLeaf),(3,NNode <$> arbitrary <*> gen (n `div` 2) <*> gen (n `div` 2))]

-- Независимый оракул: раскодировать лес в пары (значение, дети),
-- затем целиком обработать уровень и перейти к списку всех детей.
forest :: NeighborTree a -> [(a, NeighborTree a)]
forest NLeaf = []
forest (NNode x child sibling) = (x,child) : forest sibling

bfs :: [NeighborTree a] -> [a]
bfs ts = case concatMap forest ts of
  [] -> []
  row -> map fst row ++ bfs (map snd row)

testBfs :: Test
testBfs = TestList
  [ TestCase $ assertEqual "сосед раньше ребёнка" [1,2,3 :: Int] $
      toList (NNode 1 (NNode 3 NLeaf NLeaf) (NNode 2 NLeaf NLeaf))
  , TestCase $ assertEqual "пример старой домашки" [1..10 :: Int] (toList oldTree)
  , TestCase $ assertEqual "пустое дерево" ([] :: [Int]) (toList NLeaf)
  , propertyToTest "все уровни, порядок, кратность" $ \(t :: NeighborTree Int) -> toList t === bfs [t]
  , lawsToTest $ foldableLaws (Proxy @NeighborTree) ]
  where
    oldTree = NNode 1 (NNode 4 NLeaf NLeaf)
      (NNode 2 (NNode 5 (NNode 8 NLeaf NLeaf)
        (NNode 6 (NNode 9 (NNode 10 NLeaf NLeaf) NLeaf) NLeaf))
        (NNode 3 (NNode 7 NLeaf NLeaf) NLeaf))

close :: Double -> Double -> Bool
close a b = abs (a - b) < 1e-12

testIndex :: Test
testIndex = TestList
  [ TestCase $ assertEqual "нет документов" Map.empty (buildInvertedIndex [])
  , TestCase $ assertEqual "пустые документы" Map.empty $
      buildInvertedIndex [Document "empty" [], Document "punctuation" ["!?"]]
  , TestCase $ do
      let a = Document "A" ["Me me YOU!"]
          b = Document "B" ["you test"]
          idx = buildInvertedIndex [a,Document "empty" [],b]
      assertEqual "ровно нормализованные слова" ["me","test","you"] (Map.keys idx)
      let me = NE.toList (idx Map.! "me")
          you = NE.toList (idx Map.! "you")
      assertEqual "один узел на документ" [a] (map idxDocument me)
      assertBool "знаменатель учитывает повторения"
        (close (idxFrequency (NE.head (idx Map.! "me"))) (2/3))
      assertEqual "сохранён порядок документов" [a,b] (map idxDocument you)
      assertBool "частоты разных документов" (and (zipWith close (map idxFrequency you) [1/3,1/2]))
  , propertyToTest "частоты, отсутствие дублей и порядок" $ \(contents :: [[String]]) ->
      let docs = zipWith (Document . show) [0 :: Int ..] contents
          expected = Map.fromListWith (flip (++))
            [ (w,[IndexNode d (fromIntegral n / fromIntegral (sum counts))])
            | d <- docs, let counts = referenceWords (docContent d), (w,n) <- Map.toList counts ]
          actual = fmap NE.toList (buildInvertedIndex docs)
      in Map.keys actual == Map.keys expected && and
          [ length got == length want && and (zipWith same got want)
          | (w,want) <- Map.toList expected, let got = actual Map.! w ] ]
  where
    same a b = idxDocument a == idxDocument b && close (idxFrequency a) (idxFrequency b)

testFind :: Test
testFind = TestList
  [ TestCase $ assertEqual "нет слова" Nothing (findMostRelevant Map.empty "x")
  , TestCase $ assertEqual "максимум частоты" (Just "B") $
      findMostRelevant (Map.singleton "x" (node "A" 0.2 :| [node "B" 0.8,node "C" 0.5])) "x"
  , TestCase $ assertEqual "ничья независимо от порядка" (Just "A") $
      findMostRelevant (Map.singleton "x" (node "Z" 0.5 :| [node "A" 0.5])) "x"
  , TestCase $ assertEqual "единственный узел" (Just "one") $
      findMostRelevant (Map.singleton "x" (node "one" 1 :| [])) "x" ]
  where
    node name frequency = IndexNode (Document name []) frequency
