-- | Снимки условий с отдельной публичной историей.
module Admin.Publish (publish, git) where

import Data.List (dropWhileEnd)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

git :: FilePath -> [String] -> String -> IO String
git repository args input = do
  (code, output, errors) <- readCreateProcessWithExitCode
    (proc "git" args) { cwd = Just repository } input
  case code of
    ExitSuccess -> pure $ dropWhileEnd (`elem` "\r\n") output
    ExitFailure n -> ioError $ userError $
      "git " <> unwords args <> " (exit " <> show n <> "): " <> errors

publish :: FilePath -> String -> IO String
publish repository message = do
  tree <- run ["rev-parse", "main^{tree}"]
  remote <- run ["ls-remote", "public", "refs/heads/main"]
  case words remote of
    [] -> snapshot tree []
    [parent, "refs/heads/main"] -> do
      _ <- run ["fetch", "public", "refs/heads/main"]
      previousTree <- run ["rev-parse", parent <> "^{tree}"]
      if previousTree == tree then do
        putStrLn "Публичные условия уже актуальны."
        pure parent
      else snapshot tree ["-p", parent]
    _ -> ioError $ userError "Неожиданный ответ git ls-remote для public/main"
  where
    run args = git repository args ""
    snapshot tree parents = do
      commit <- git repository (["commit-tree", tree] <> parents) (message <> "\n")
      -- Без --force: конкурентное изменение публичной ветки останавливает push.
      _ <- run ["push", "public", commit <> ":refs/heads/main"]
      putStrLn $ "Публичный снимок условий: " <> commit
      pure commit
