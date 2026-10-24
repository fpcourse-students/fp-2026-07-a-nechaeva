module Test.Run (NamedTests, testMain, nameTests) where

import Control.Monad (forM, unless, when)
import Data.List qualified as List
import Data.Maybe (fromMaybe, isJust)
import System.Environment (lookupEnv, getArgs)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Test (..), Counts (..))
import Test.HUnit.Base qualified as HU
import Test.Manifest

type NamedTests = [(String, Test)]

-- | 'TestTodo' — каждая проверка упёрлась в заглушку @todo@ или дырку:
-- к задаче не приступали, и это не то же самое, что 'TestFailed'.
data TestStatus = TestPassed | TestPartial Float | TestFailed | TestTodo deriving Eq
data TestResult = TestResult { testName :: String, testStatus :: TestStatus }
type TestReport = [TestResult]

-- | Строка итогового отчёта: задача из манифеста либо запущенный тест вне манифеста.
data TaskResult = TaskResult { taskId :: TaskId, taskLevel :: Maybe Int, taskStatus :: TaskStatus }
type TaskReport = [TaskResult]

instance Semigroup Counts where
  c <> c' = Counts
    { errors = errors c + errors c'
    , failures = failures c + failures c'
    , tried = tried c + tried c'
    , cases = cases c + cases c'
    }

instance Monoid Counts where
  mempty = Counts 0 0 0 0

-- | Запускает все тесты (или только перечисленные в аргументах), печатает отчёт
-- и пишет его в файлы из @HASKELL_TEST_REPORT@ / @HASKELL_TEST_REPORT_JSON@.
-- Код выхода — закрыт ли уровень 1 (так же красится CI);
-- с @HASKELL_TEST_STRICT@ — все ли задачи DONE (проверка ветки с решениями).
-- С @HASKELL_TEST_STARTER@ — все ли задачи ещё TODO/PARTIAL (проверка заготовки).
testMain :: NamedTests -> IO ()
testMain tests = do
  testFilters <- getTestFilters
  strict <- isJust <$> lookupEnv "HASKELL_TEST_STRICT"
  starter <- isJust <$> lookupEnv "HASKELL_TEST_STARTER"
  when (starter && (strict || not (null testFilters))) do
    putStrLn "Проверка заготовки требует полного прогона без HASKELL_TEST_STRICT."
    exitFailure
  -- @HASKELL_TASKS@ подменяет манифест; несуществующий путь даёт пустой манифест
  -- (так `make check FILE=…` проверяет файл, к которому TASKS не относится).
  manifestPath <- fromMaybe "TASKS" <$> lookupEnv "HASKELL_TASKS"
  manifest <- fromMaybe emptyManifest <$> readManifest manifestPath
  when starter do
    let expected = manifestTasks manifest
    unless (not (null expected) && List.sort (map fst tests) == List.sort expected) do
      putStrLn "Проверка заготовки: нужен непустой TASKS и ровно один набор тестов для каждой задачи."
      exitFailure
  -- Опечатка в TASKS дала бы задачу, которая вечно TODO, и уровень 1 никогда бы не закрылся.
  let unknown = filter (`notElem` map fst tests) $ manifestTasks manifest
  unless (null unknown) do
    putStrLn $ "TASKS mentions tasks that have no tests: " <> unwords unknown
    exitFailure
  putStrLn $ "Executing: " <>
    if null testFilters then "all tests" else List.intercalate ", " testFilters
  (counts, report) <- runTests $ filterTests testFilters tests
  lookupEnv "HASKELL_TEST_REPORT" >>= maybe (pure ()) \filePath ->
    appendFile filePath $ showMachine report
  let taskReport = collectTaskReport manifest report
      closed = level1Closed manifest (map taskId taskReport) $ statusOf taskReport
      missing = level1Missing manifest (map taskId taskReport) $ statusOf taskReport
  lookupEnv "HASKELL_TEST_REPORT_JSON" >>= maybe (pure ()) \filePath ->
    writeFile filePath $ showJson taskReport closed
  putStrLn $ "\n" <> showCounts counts <> "\n"
  putStr $ showTaskReport taskReport
  putStrLn $ "Level 1: " <> if closed then "closed" else "open (missing: " <> unwords missing <> ")"
  let allDone = all ((== TaskDone) . taskStatus) taskReport
  if starter then case checkStarter manifest taskReport of
    Left message -> putStrLn ("Проверка заготовки: " <> message) >> exitFailure
    Right () -> do
      putStrLn $ "Студенческий шаблон: " <> show (length taskReport) <> " задач, обязательные не закрыты — OK"
      exitSuccess
  else if (if strict then allDone else closed) then exitSuccess else exitFailure
  where
    getTestFilters :: IO [String]
    getTestFilters = concatMap words <$> getArgs

    filterTests :: [String] -> NamedTests -> NamedTests
    filterTests names = filter \(name, _) -> null names || name `elem `names

    statusOf :: TaskReport -> TaskId -> TaskStatus
    statusOf taskReport name = maybe TaskTodo taskStatus $ List.find ((== name) . taskId) taskReport

checkStarter :: Manifest -> TaskReport -> Either String ()
checkStarter manifest report = do
  unless (all (isStarterStatus . taskStatus) report) $
    Left "в заготовке допустимы только статусы TODO и PARTIAL"
  when (level1Closed manifest (map taskId report) statusOf) $
    Left "обязательный уровень уже закрыт"
  where
    statusOf name = maybe TaskTodo taskStatus $ List.find ((== name) . taskId) report
    isStarterStatus = \case
      TaskTodo -> True
      TaskPartial _ -> True
      _ -> False

-- | Нумерует тесты уровня: @nameTests 2@ даёт задачи @2.1@, @2.2@, …
-- Первое число идентификатора — уровень задачи, см. "Test.Manifest".
nameTests :: Int -> [Test] -> NamedTests
nameTests iLevel = map wrapToTestLabel . zipWith mkNamedTest [1 :: Int ..]
  where
    wrapToTestLabel (label, test) = (label, TestLabel label test)
    mkNamedTest iTask test = (show iLevel <> "." <> show iTask, test)

-- | Статус по счётчикам HUnit и числу проверок, упёршихся в заглушку.
statusFromCounts :: Counts -> Int -> TestStatus
statusFromCounts Counts {..} todos
  | errors == 0 && failures == 0 = TestPassed
  | todos == tried = TestTodo
  | errors + failures < tried = TestPartial $
      1 - fromIntegral (errors + failures) / fromIntegral tried
  | otherwise = TestFailed

taskStatusFromTest :: TestStatus -> TaskStatus
taskStatusFromTest = \case
  TestPassed -> TaskDone
  TestPartial percent -> TaskPartial percent
  TestFailed -> TaskFailed
  TestTodo -> TaskTodo

showCounts :: Counts -> String
showCounts Counts {..} = concat
  [ "Cases: ", show cases
  , "  Tried: ", show tried
  , "  Errors: ", show errors
  , "  Failures: ", show failures
  ]

showMachine :: TestReport -> String
showMachine = List.intercalate "\n" . map showResult
  where
    showResult TestResult {..} = testName <> "=" <> case testStatus of
      TestPassed -> "DONE"
      TestPartial percent -> "PARTIAL " <> show percent
      TestFailed -> "FAILED"
      TestTodo -> "TODO"

-- | Задачи манифеста в его порядке (незапущенные получают 'TaskTodo'),
-- затем запущенные тесты, которых в манифесте нет. Уровень — из идентификатора.
collectTaskReport :: Manifest -> TestReport -> TaskReport
collectTaskReport manifest report = map fromManifest (manifestTasks manifest) ++ map fromRun unlisted
  where
    fromManifest name = TaskResult
      { taskId = name
      , taskLevel = levelOfTask name
      , taskStatus = maybe TaskTodo (taskStatusFromTest . testStatus) $
          List.find ((== name) . testName) report
      }
    unlisted = filter (\TestResult {..} -> testName `notElem` manifestTasks manifest) report
    fromRun TestResult {..} = TaskResult
      { taskId = testName
      , taskLevel = levelOfTask testName
      , taskStatus = taskStatusFromTest testStatus
      }

showTaskReport :: TaskReport -> String
showTaskReport = unlines . map showTask
  where
    showTask TaskResult {..} = List.intercalate "  "
      [ padRight 6 taskId
      , "level " <> maybe "-" show taskLevel
      , showStatus taskStatus
      ]
    showStatus = \case
      TaskDone -> "DONE"
      TaskPartial percent -> "PARTIAL " <> show (floor $ percent * 100) <> "%"
      TaskFailed -> "FAILED"
      TaskTodo -> "TODO"
    padRight n s = s <> replicate (n - length s) ' '

-- | Без aeson: структура плоская, а идентификаторы состоят из цифр и точек.
showJson :: TaskReport -> Bool -> String
showJson taskReport closed = jsonObject
  [ ("tasks", jsonArray $ map showTask taskReport)
  , ("level1_closed", if closed then "true" else "false")
  ]
  where
    showTask TaskResult {..} = jsonObject $
      [ ("id", jsonString taskId)
      , ("level", maybe "null" show taskLevel)
      , ("status", jsonString $ statusName taskStatus)
      ] ++ case taskStatus of
        TaskPartial percent -> [("progress", show percent)]
        _ -> []
    statusName = \case
      TaskDone -> "DONE"
      TaskPartial _ -> "PARTIAL"
      TaskFailed -> "FAILED"
      TaskTodo -> "TODO"
    jsonObject fields = "{" <> List.intercalate "," [jsonString key <> ":" <> value | (key, value) <- fields] <> "}"
    jsonArray items = "[" <> List.intercalate "," items <> "]"
    jsonString s = "\"" <> concatMap escape s <> "\""
    escape = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      c -> [c]

runTests :: NamedTests -> IO (Counts, TestReport)
runTests tests = collectReport <$> forM tests \(name, test) -> do
  putStrLn $ "Running test \"" <> name <> "\":"
  -- Состояние прогона — число проверок, упёршихся в заглушку или дырку.
  (counts, todos) <- HU.performTest reportStart reportError reportFailure (0 :: Int) test
  let status = statusFromCounts counts todos
      result = TestResult { testName = name, testStatus = status }
  reportSummary status
  pure (counts, result)
  where
    reportStart _ todos = pure todos -- per test case
    reportError loc msg state todos = do
      -- 'TodoException' показывается как "Not implemented: …"
      let isTodo = "Not implemented" `List.isInfixOf` msg
      reportProblem (if isTodo then "[TODO] " else "[ERROR] ") loc msg state
      pure $ if isTodo then todos + 1 else todos
    reportFailure loc msg state todos = todos <$ reportProblem "[FAILURE] " loc msg state
    reportProblem prefix _ msg HU.State{..} = putStr $ padLines 4 $
      prefix <> showPath path <> " " <> msg <> if '\n' `elem` msg then "\n" else ""
    reportSummary status = putStr $ padLines 4 $ case status of
      TestPassed -> "Done :)"
      TestPartial percent -> "In progress, " <> show (floor $ percent * 100) <> "% of tests pass :|"
      TestFailed -> "Nothing here :("
      TestTodo -> "Not started yet"

padLines :: Int -> String -> String
padLines nSpaces = unlines . map (replicate nSpaces ' ' ++) . lines

collectReport :: [(Counts, TestResult)] -> (Counts, TestReport)
collectReport = sequenceA

showPath :: HU.Path -> String
showPath = List.intercalate ":" . reverse . map \case
  HU.ListItem n -> show n
  HU.Label label -> label
