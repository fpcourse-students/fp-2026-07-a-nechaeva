{-# LANGUAGE CPP #-}
import Data.Maybe (fromMaybe)
import Lambda.Console (setupConsole)
import System.Environment (lookupEnv)
import Test.Lambda (lambdaTests)
import Test.Prelude

#ifndef LAMBDA
import SpecLevel1 qualified
import SpecLevel2 qualified
import SpecLevel3 qualified
#endif

-- | Задачи λ-домашки (если есть src/hw.lam или файл из @LAMBDA_FILE@, так
-- `make check FILE=…` проверяет учебный файл) и задачи трёх уровней на Haskell.
-- В λ-домашке (флаг @lambda@ в homework.cabal, макрос @LAMBDA@) Haskell-уровней нет.
main :: IO ()
main = do
  -- До любого чтения файлов и вывода: UTF-8 и консоль Windows, см. "Lambda.Console".
  setupConsole
  file <- fromMaybe "src/hw.lam" <$> lookupEnv "LAMBDA_FILE"
  lambda <- lambdaTests [file]
  testMain $ lambda ++ haskell

haskell :: NamedTests
#ifdef LAMBDA
haskell = []
#else
haskell = SpecLevel1.tests ++ SpecLevel2.tests ++ SpecLevel3.tests
#endif
