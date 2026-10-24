module Test.Utils where

import Control.DeepSeq
import Control.Exception
import Test.HUnit (assertFailure)

domain :: (Enum a, Bounded a) => [a]
domain = [minBound .. maxBound]

instance Show (a -> b) where
  show _ = "<fun>"

instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
  xs == ys = map xs domain == map ys domain

assertThrows :: forall exception a . (Show a, NFData a, Exception exception) => a -> IO ()
assertThrows comp = try (evaluate (force comp)) >>= \case
  Left e -> case fromException @exception e of
    Just _ -> pure ()
    Nothing -> throw e
  Right result -> assertFailure $ "Expected exception, got result: " <> show result
