module Test.Lazy where

import Control.Exception

data TooEagerException = TooEagerException
instance Exception TooEagerException
instance Show TooEagerException where
  show TooEagerException = "Форсировали ненужные данные"

infinite :: forall a. a
infinite = throw TooEagerException
