{-
                   ___  ___
         \  \  /`\ \  \ \  \ \  \
          \__\ \__\ \__' \__' \__\
           \  \ \  \ \    \     \
            \  \ \  \ \    \     \
                    ___         ___  ___
\  \  /`\ \    \    \  \ \    \ \    \    \.  \
 \__\ \__\ \    \    \  \ \  . \ \__  \__  \`\ \
  \  \ \  \ \    \    \  \ \ |`\\ \    \    \ `\\
   \  \ \  \ \___ \___ \__\ \|  `\ \___ \___ \  `\
-}

-- | Домашка 7. Свёртки: обязательные задачи.
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Level1 where

import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Examples qualified as E
import MetaUtils (todo, todoImpl)


-- 1.1. До запуска запишите результаты литералами, без вычислений из условия.

-- foldr (-) 0 [1, 2, 3] :: Integer
predictedRight :: Integer
predictedRight = todo "1.1 foldr"

-- foldl (-) 0 [1, 2, 3] :: Integer
predictedLeft :: Integer
predictedLeft = todo "1.1 foldl"

-- foldMap (\x -> [x,x]) (Just (2 :: Integer))
predictedFoldMap :: [Integer]
predictedFoldMap = todo "1.1 foldMap"


-- 1.2. Найдите контрпример к E.brokenIncome в src/Examples.hs.
-- Сдайте (заработано, расходы), на котором ответ отличается от
-- «заработано минус сумма всех расходов». Расходы и заработок неотрицательны.

incomeCounterexample :: (Integer, [Integer])
incomeCounterexample = todo "1.2"


-- 1.3. Завершите поиск элемента через foldr.
-- Если элемент найден в конечном префиксе, хвост не должен вычисляться:
-- elem' 4 [1..] == True. На конечном списке без элемента верните False.
-- Для бесконечного списка без элемента завершение не требуется.

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y rest -> todo "1.3 шаг") False


-- 1.4. Прочитайте E.sizeViaFoldMap. Через foldMap и Sum подсчитайте
-- элементы, удовлетворяющие предикату, в любом Foldable-контейнере.

countMatching :: (Foldable t, Num n) => (a -> Bool) -> t a -> n
countMatching p = getSum . foldMap contribution
  where
    contribution x
      | p x = todo "1.4 вклад подходящего элемента"
      | otherwise = Sum 0


-- 1.5. Напишите полный инстанс Foldable самостоятельно:
-- выберите метод, необходимые ограничения и напишите instance ... where.
-- На практике было два поддерева; здесь узел хранит список детей.
-- Сначала корень, затем поддеревья по порядку, каждое целиком (DFS).
-- Leaf пуст; Node x [] содержит x. deriving Foldable использовать нельзя.
-- Законы Semigroup, Monoid и Foldable — лекция 7, слайды 19, 23, 36.

data Tree a = Leaf | Node a [Tree a]
  deriving (Eq, Show)

todoImpl ''Foldable ''Tree


-- 1.6. Самостоятельно составьте цепочку преобразований через (&):
-- посчитайте вхождения каждого слова в списке строк.
-- Используйте данный E.tokens для нормализации, Map для результата.
-- Слова регистронезависимы, пунктуация — разделитель, цифры сохраняются.
-- Пример: ["Alice's", "ALICE!"] -> [("alice", 2), ("s", 1)].
-- Пустой ввод или строки только из пунктуации дают Map.empty.
-- Метод накопления выберите сами; явная рекурсия по списку не нужна.
-- После решения 2.6 запустите из корня репозитория подсчёт слов
-- в «Алисе в Стране чудес»:
--   make run ARGS=wc < text/alice.txt

wordCount :: [String] -> Map String Int
wordCount = todo "1.6"
