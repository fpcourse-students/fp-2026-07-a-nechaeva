module PublishSpec (tests) where

import Admin.Publish (git, publish)
import Control.Exception (bracket)
import Control.Monad (void)
import System.Directory (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.HUnit

tests :: Test
tests = TestLabel "Публичная история" $ TestList
  [ TestLabel "Первый снимок не содержит приватных коммитов" $ TestCase $
      withRepositories \private public secret -> do
        first <- publish private "Выдать ДЗ"
        assertEqual "единственный публичный коммит" first =<< run public ["rev-list", "--all"]
        assertEqual "условия" "todo" =<< run public ["show", "main:task.txt"]
        assertSecretAbsent public secret
  , TestLabel "Обновление продолжает только публичную историю" $ TestCase $
      withRepositories \private public secret -> do
        first <- publish private "Первый снимок"
        writeFile (private </> "task.txt") "updated todo\n"
        writeFile (private </> "README.md") "Условия\n"
        commit private "Обновить условия"
        second <- publish private "Второй снимок"
        ancestry <- lines <$> run public ["rev-list", "main"]
        assertEqual "родитель только публичный" [second, first] ancestry
        expectedTree <- run private ["rev-parse", "main^{tree}"]
        assertEqual "точный снимок условий" expectedTree =<< run public ["rev-parse", "main^{tree}"]
        assertSecretAbsent public secret
        assertEqual "повторный выпуск не создаёт коммит" second =<< publish private "Без изменений"
  ]

run :: FilePath -> [String] -> IO String
run repository args = git repository args ""

commit :: FilePath -> String -> IO ()
commit repository message = do
  void $ run repository ["add", "-A"]
  void $ run repository ["commit", "-m", message]

assertSecretAbsent :: FilePath -> String -> Assertion
assertSecretAbsent repository secret = do
  (code, _, _) <- readCreateProcessWithExitCode
    (proc "git" ["cat-file", "-e", secret]) { cwd = Just repository } ""
  assertBool "приватный объект не должен попасть в публичный репозиторий" (code /= ExitSuccess)

-- Уникальное имя резервируется временным файлом до конца проверки.
withRepositories :: (FilePath -> FilePath -> String -> IO a) -> IO a
withRepositories action = do
  temporary <- getTemporaryDirectory
  bracket (openTempFile temporary "homework-publication") cleanup \(marker, handle) -> do
    hClose handle
    let root = marker <> ".dir"
        private = root </> "private"
        public = root </> "public.git"
    createDirectory root
    void $ run root ["init", "-b", "main", private]
    void $ run root ["init", "--bare", "-b", "main", public]
    void $ run private ["config", "user.name", "Publication test"]
    void $ run private ["config", "user.email", "test@example.invalid"]
    void $ run private ["config", "commit.gpgSign", "false"]
    void $ run private ["remote", "add", "public", public]
    writeFile (private </> "answer.txt") "private solution\n"
    commit private "Приватный эталон"
    secret <- run private ["rev-parse", "HEAD"]
    removeFile (private </> "answer.txt")
    writeFile (private </> "task.txt") "todo\n"
    commit private "Студенческая заготовка"
    action private public secret
  where
    cleanup (marker, handle) = do
      hClose handle
      removePathForcibly (marker <> ".dir")
      removeFile marker
