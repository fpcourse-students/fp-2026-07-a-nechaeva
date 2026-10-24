-- | Челленджи. Эти задачи не входят в зачёт домашки.
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Level3 where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Level1 (wordCount)
import MetaUtils (todo)


-- 3.1. Дерево «левый ребёнок, правый сосед». Непустой корневой
-- сосед тоже принадлежит первому уровню. Обход — по уровням (BFS),
-- слева направо, без Leaf. На каждом уровне учитывайте всех соседей.
-- Пример NNode 1 (NNode 3 NLeaf NLeaf) (NNode 2 NLeaf NLeaf)
-- должен дать [1,2,3], а не [1,3,2].

data NeighborTree a
  = NLeaf
  | NNode a (NeighborTree a) (NeighborTree a)
  deriving (Eq, Show)

-- Фейдинг: pending — обратный список детей следующего уровня.
-- Текущий уровень читается из первого списка. Завершите ветку NNode.

instance Foldable NeighborTree where
  foldMap f tree = go [tree] []
    where
      go [] [] = mempty
      go [] pending = go (reverse pending) []
      go (NLeaf : ts) pending = go ts pending
      go (NNode x child sibling : ts) pending = todo "3.1 шаг BFS"


-- 3.2. Инвертированный индекс: слово -> непустой список документов.
-- Нормализация та же, что у wordCount. Частота = число вхождений слова
-- / ОБЩЕЕ число слов документа после нормализации (с повторами).
-- Один документ даёт не больше одного IndexNode на слово.
-- Пустой документ не добавляет узлов; деления на ноль не должно быть.
-- Порядок узлов совпадает с порядком документов на входе.
-- Имена входных документов уникальны. Реализуйте цепочкой преобразований.
-- Вместе с решениями 1.6 и 3.3 постройте индекс семи книг о Гарри Поттере
-- и выполните поиск трёх слов (команда запускается из корня репозитория):
--   make run ARGS='find hermione basilisk lol'

data Document = Document
  { docName :: String
  , docContent :: [String]
  } deriving (Eq, Show)

data IndexNode = IndexNode
  { idxDocument :: Document
  , idxFrequency :: Double
  } deriving (Eq, Show)

type Index = Map String (NonEmpty IndexNode)

buildInvertedIndex :: [Document] -> Index
buildInvertedIndex = todo "3.2"


-- 3.3. Верните имя документа с наибольшей частотой данного слова.
-- При равной частоте выберите лексикографически наименьшее имя.
-- Слово уже нормализовано (нижний регистр, без пунктуации).
-- Если ключа нет, верните Nothing. Эта задача проверяется отдельно
-- от 3.2: тесты передают готовый индекс.
-- Вместе с решениями 1.6 и 3.2 проверьте поиск по семи книгам:
--   make run ARGS='find hermione basilisk lol'
-- Для поиска по выбранному тексту укажите файл явно:
--   make run ARGS='find-in alice text/alice.txt'
-- Команды запускаются из корня репозитория. В find-in после слова
-- можно указать несколько файлов, чтобы сравнить частоты в них.

findMostRelevant :: Index -> String -> Maybe String
findMostRelevant = todo "3.3"
