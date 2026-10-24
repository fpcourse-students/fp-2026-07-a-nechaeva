{-
  '.  `~~`~`~~^`~~`~^~^`~~`~~`~^~`.   .`     /
    `> ' . ' ". _ '-"`  .  ` ' .   | .    ' /
   .' ,'~^~^~^`^~~^`^~`~~~^~~~^;       ' ' |
   .-'                          \ ` : ` . "|_
                                | | ,  | : | \
                                | "   `    |  \
                                | , :   |  |  /~--,__
                                |     `  , |  \__,--~'
                        ("-,_   |" `  | `, |   /
                         "-,_"-,|, _    ,  |__/
                             "-,|   \\______/ \
                                |,  | / _\___/ \
                               /| "  \ \/\  ||   \
                              / |     \   \,||    \
                        __.--~\ |`  `  \__/ ||     ),_
                       `~--.__/ | , |    :/  \   /-,_"-,_    __
                             \  |   `,   /=,'/ ('    `-,_"-,/_ /~=,_
                              \_| |   __/=/ |   \        "-/-,//~=,_~=,_
                              / |`   (__ / ('    \         \_//_~=,_~=,
                             / \\______//   \     )         '=,_~=,_~=
                            /   ___/_ \ | .| `\__(              ~=,_~
                           /    |   /\/ /" |
                        _.(     | ./   /   |
              __    _.-"_.-\   /|_ \__/ ' '|
         _.=~\ _\.-"_.-'    `) \  \:   | . |
     _.=~_.=~\\.-\-"        /   `.=\  .'   |
      .=~_.=~_\\_/         /    | \=\__  | |
       =~_.=~_.=`         (     `) \ __)  '|
        ~_.=~              )__/'| '   "   '|
                                | . " | ' .|
 '-"_'"-'_"'-_'"-_''_"-"-_"-\ \/'   '   . '`\/"/- '"-_-"'_ _-"'__-"'_
-_ --"-"_---_""-_'"--"'_-"-'\\/.\\' / /".\,//\//'-"-_'"-"'_'"-"''"-"'_
   -"-'_-"_-"-_"-"'_'-"-"_``"-`"_`'""-`''""'-_'"-"-_"'"-   '"- _'"- _
-}

-- | К сессии. На доске может быть вариация одной из этих задач.
module Level2 where

import Data.List (unfoldr)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (Down (..))
import MetaUtils (todo, todoImpl)


-- 2.1. Специализированные свёртки.

data Nat = Z | S Nat deriving (Eq, Show, Ord)

foldNat :: a -> (a -> a) -> Nat -> a
foldNat z s = \case
  Z -> z
  S n -> s (foldNat z s n)

-- Для Left верните сумму двух Nat, для Right — длину строки как Nat.
-- В решении и вспомогательных функциях используйте только foldr,
-- uncurry, either, foldNat, Z, S и комбинаторы const, id, (.), ($).
-- Без сопоставления с образцом и явной рекурсии.

sumOrLength :: Either (Nat, Nat) String -> Nat
sumOrLength = todo "2.1"


-- 2.2. Реализуйте map через unfoldr, не конструируя результат через [] и (:).
-- Сопоставлять входной список с образцом можно. Сохраните продуктивность:
-- take 3 (map' (+1) [1..]) == [2,3,4].

map' :: (a -> b) -> [a] -> [b]
map' = todo "2.2"


-- 2.3. Разное: словарь как частичная функция.
-- Выберите способ решения сами. Новая запись заменяет прежнее значение.
-- myInsertMany обрабатывает контейнер слева направо: последний дубликат
-- побеждает. Остальные ключи сохраняются. Удаление отсутствующего безопасно.

newtype MyMap k v = MyMap { myLookup :: k -> Maybe v }

myEmpty :: MyMap k v
myEmpty = todo "2.3 пустой словарь"

myInsert :: Eq k => k -> v -> MyMap k v -> MyMap k v
myInsert = todo "2.3 вставка"

myInsertMany :: (Eq k, Foldable t) => MyMap k v -> t (k, v) -> MyMap k v
myInsertMany = todo "2.3 массовая вставка"

myDelete :: Eq k => k -> MyMap k v -> MyMap k v
myDelete = todo "2.3 удаление"

myDeleteMany :: (Eq k, Foldable t) => MyMap k v -> t k -> MyMap k v
myDeleteMany = todo "2.3 массовое удаление"


-- 2.4. Напишите полные инстансы Semigroup и Monoid.
-- При объединении общий ключ берётся из ЛЕВОГО словаря, иначе — из правого.
-- Ограничения на тип значения не нужны: значения не объединяются.
-- Нейтральный элемент — словарь, возвращающий Nothing для любого ключа.

todoImpl ''Semigroup ''MyMap
todoImpl ''Monoid ''MyMap


-- 2.5. Напишите полный Foldable (MyMap k) для (Enum k, Bounded k).
-- Обходите ключи в порядке [minBound .. maxBound], пропуская Nothing.
-- Каждый существующий ключ даёт одно значение, даже если значения равны.

todoImpl ''Foldable ''MyMap


-- 2.6. Разное: отформатируйте результат wordCount.
-- Сначала исключите слова длиной <= smallWordsSize, затем сортируйте
-- по убыванию числа вхождений, при равенстве — лексикографически.
-- После фильтрации и сортировки возьмите topSize слов.
-- Строка имеет вид "слово : N\n". Неположительный topSize даёт "".
-- Вместе с решением 1.6 проверьте вывод на «Алисе в Стране чудес».
-- Из корня репозитория выполните:
--   make run ARGS='wc text/alice.txt'
-- Приложение выведет 30 самых частых слов длиной больше трёх букв.

data WordCountShow = WordCountShow
  { topSize :: Int
  , smallWordsSize :: Int
  } deriving (Eq, Show)

stdWcShow :: WordCountShow
stdWcShow = WordCountShow { topSize = 10, smallWordsSize = 3 }

wordCountShow :: WordCountShow -> Map String Int -> String
wordCountShow = todo "2.6"


-- 2.7. Разное: сохраните значения Right в исходном порядке,
-- пропустите Left. Метод решения выберите сами.
-- collectRights [Right 2, Left "ошибка", Right 1] == [2,1].
-- Конечный префикс результата должен работать на бесконечном входе.

collectRights :: [Either e a] -> [a]
collectRights = todo "2.7"
