module Main where

import Admin.Publish (publish)
import Control.Monad (void)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = getArgs >>= \case
  ["publish", "--message", message] -> void $ publish "." message
  ["publish"] -> void $ publish "." "Обновить условия домашнего задания"
  _ -> die "Использование: homework-admin publish [--message TEXT]"
