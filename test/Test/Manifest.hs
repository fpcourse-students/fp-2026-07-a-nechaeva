-- | Манифест задач домашки: файл @TASKS@ в корне пакета.
--
-- Уровень задачи — число до первой точки в её идентификаторе: @1.x@ — обязательные,
-- @2.x@ — к сессии, @3.x@ — челлендж. Манифест задаёт только порядок задач, подписи
-- для человека и правила «любые k из n»; без файла задачи идут в порядке тестов.
--
-- Грамматика (пустые строки и строки, начинающиеся с @--@, игнорируются):
--
-- > task 1.1 (15 мин)
-- > task 1.2 (30 мин)
-- > rule any 2 of: 1.1 1.2 1.3
-- > task 2.1 (10 мин)
module Test.Manifest
  ( TaskId, TaskStatus (..), Rule (..), Manifest (..)
  , emptyManifest, parseManifest, readManifest
  , levelOfTask, level1Missing, level1Closed
  ) where

import Control.Exception (throwIO, try)
import Control.Monad (foldM, unless, when)
import Data.List qualified as List
import Data.Maybe (isNothing)
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)

type TaskId = String

-- | Статус задачи в итоговом отчёте.
-- В отличие от результата прогона, у задачи из манифеста может не быть результата вовсе.
data TaskStatus = TaskDone | TaskPartial Float | TaskFailed | TaskTodo deriving (Eq, Show)

-- | @rule any k of: ids@ — из перечисленных задач достаточно закрыть @k@.
data Rule = AnyOf Int [TaskId] deriving (Eq, Show)

data Manifest = Manifest
  { manifestTasks :: [TaskId] -- ^ в порядке объявления
  , manifestRules :: [Rule]
  } deriving (Eq, Show)

emptyManifest :: Manifest
emptyManifest = Manifest [] []

-- | Уровень задачи по идентификатору: @"2.3"@ → @Just 2@.
levelOfTask :: TaskId -> Maybe Int
levelOfTask = readMaybe . takeWhile (/= '.')

parseManifest :: String -> Either String Manifest
parseManifest = foldM step emptyManifest . meaningful . zip [1 :: Int ..] . lines
  where
    meaningful = filter (\(_, line) -> not $ null line || "--" `List.isPrefixOf` line) . map (fmap strip)
    strip = List.dropWhileEnd isBlank . dropWhile isBlank
    isBlank c = c `elem` " \t\r"

    step manifest@Manifest {..} (n, line) = case words line of
      ("task" : taskId : _) -> do
        when (isNothing $ levelOfTask taskId) $
          failAt n $ "task id must start with a level number: " <> taskId
        when (taskId `elem` manifestTasks) $ failAt n $ "duplicate task: " <> taskId
        pure manifest { manifestTasks = manifestTasks ++ [taskId] }
      ("rule" : "any" : count : "of:" : taskIds) -> do
        k <- maybe (failAt n $ "bad count: " <> count) pure $ readMaybe count
        unless (k >= 1 && k <= length taskIds) $
          failAt n $ "rule wants " <> show k <> " of " <> show (length taskIds) <> " tasks"
        case filter (`notElem` manifestTasks) taskIds of
          [] -> pure ()
          unknown -> failAt n $ "rule mentions tasks not declared above: " <> unwords unknown
        unless (length (List.nub $ map levelOfTask taskIds) == 1) $
          failAt n "rule mixes tasks of different levels"
        pure manifest { manifestRules = manifestRules ++ [AnyOf k taskIds] }
      _ -> failAt n $ "unrecognised line: " <> line

    failAt n msg = Left $ "TASKS:" <> show n <> ": " <> msg

-- | Читает манифест из файла. Если файла нет — 'Nothing'; если он не разбирается — ошибка.
readManifest :: FilePath -> IO (Maybe Manifest)
readManifest path = try (readFile path) >>= \case
  Left e | isDoesNotExistError e -> pure Nothing
         | otherwise -> throwIO e
  Right contents -> either (ioError . userError) (pure . Just) $ parseManifest contents

-- | Задачи уровня 1 из перечисленных, которые ещё не закрыты с учётом правил «любые k из n».
-- Задача считается закрытой, если она 'TaskDone' либо входит в правило,
-- у которого закрыто не меньше @k@ перечисленных задач.
level1Missing :: Manifest -> [TaskId] -> (TaskId -> TaskStatus) -> [TaskId]
level1Missing Manifest {..} taskIds statusOf =
  filter (not . satisfied) $ filter ((== Just 1) . levelOfTask) taskIds
  where
    satisfied taskId = done taskId || any (coveredBy taskId) manifestRules
    coveredBy taskId (AnyOf k ids) = taskId `elem` ids && length (filter done ids) >= k
    done taskId = statusOf taskId == TaskDone

-- | Уровень 1 закрыт, когда все его задачи закрыты. Если задач уровня 1 нет — закрыт.
level1Closed :: Manifest -> [TaskId] -> (TaskId -> TaskStatus) -> Bool
level1Closed manifest taskIds = null . level1Missing manifest taskIds
