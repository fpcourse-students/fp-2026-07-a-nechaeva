-- | Мост между проверялкой λ-домашек и HUnit-раннером шаблона.
--
-- Каждая задача файла @.lam@ становится одним именованным тестом
-- (имя = идентификатор задачи, как в @TASKS@), а каждая её проверка —
-- отдельным @TestCase@. Так частично решённая задача получает статус PARTIAL
-- средствами обычного раннера, а проверка, упёршаяся в дырку, бросает
-- 'TodoException' — как заглушка @todo@ в Haskell-домашках, и раннер считает
-- задачу TODO, если все её проверки такие.
-- Файлы, которых нет, пропускаются: в Haskell-домашках @src/hw.lam@ отсутствует.
module Test.Lambda (lambdaTests) where

import Control.Exception (throwIO, ErrorCall (..))
import Control.Monad (filterM)
import Lambda.Check (CheckResult (..), TaskResult (..), checkFile, isHoleMessage)
import System.Directory (doesFileExist)
import Test.HUnit (Test (..), assertFailure)
import Test.Run (NamedTests)
import TodoException (TodoException (..))

-- | Тесты всех задач существующих файлов из списка, в порядке файлов.
-- Ошибка разбора файла (или его импорта) — исключение: это ошибка
-- всего файла, а не одной задачи.
lambdaTests :: [FilePath] -> IO NamedTests
lambdaTests files = do
  present <- filterM doesFileExist files
  concat <$> mapM one present
  where
    one file = do
      result <- checkFile file
      case result of
        Left err -> throwIO (ErrorCall (file <> ":\n" <> err))
        Right tasks -> pure (map toTest tasks)

    toTest task =
      let name = trId task
          cases = map toCase (trChecks task)
      in  (name, TestLabel name (TestList cases))

    toCase (CheckResult label outcome) = TestLabel label $ TestCase $
      case outcome of
        Right () -> pure ()
        Left msg
          | isHoleMessage msg -> throwIO (TodoException msg)
          | otherwise -> assertFailure msg
