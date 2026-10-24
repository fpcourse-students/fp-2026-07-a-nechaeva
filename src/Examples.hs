-- | Данный код для чтения. Менять этот модуль не нужно.
module Examples where

import Data.Char qualified as Char
import Data.Monoid (Sum (..))

-- Разобранный пример: каждый элемент превращается в единицу,
-- моноид Sum складывает их, getSum снимает обёртку.
sizeViaFoldMap :: Foldable t => t a -> Int
sizeViaFoldMap = getSum . foldMap (Sum . const 1)

-- Попытка вычесть все расходы из заработанного. Здесь есть ошибка.
brokenIncome :: Integer -> [Integer] -> Integer
brokenIncome earned = foldr (-) earned

-- Общая подготовка текста для задач 1.6, 3.2 и 3.3.
-- Пунктуация становится пробелами; остальные символы сохраняются
-- и приводятся к нижнему регистру. words разбивает по пробельным символам.
tokens :: [String] -> [String]
tokens = concatMap (words . map normalize)
  where
    normalize c
      | Char.isPunctuation c = ' '
      | otherwise = Char.toLower c
