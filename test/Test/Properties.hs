module Test.Properties (propertyToTest, propertyToTestIO, lawsToTest, filterLaws) where

import Control.Exception
import Test.HUnit (Test (..), assertFailure)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Classes.Base qualified as QC
import MetaUtils

propertyToTest :: QC.Testable prop => String -> prop -> Test
propertyToTest description = propertyToTestIO description . pure

propertyToTestIO :: QC.Testable prop => String -> IO prop -> Test
propertyToTestIO description property = TestCase $
  property >>= QC.quickCheckWithResult args >>= \case
    QC.Failure { theException = Just exception } ->
      case fromException @TodoException exception of
        Just _ -> throwIO exception
        Nothing -> assertFailure $ "tested " <> description <> ", but\n" <> show exception
    QC.Failure { theException = Nothing, .. } ->
      assertFailure $ "tested " <> description <> ", but\n" <> output
    _ -> pure ()
  where
    args = QC.stdArgs { QC.chatty = False, QC.maxSuccess = 150 }

lawsToTest :: QC.Laws -> Test
lawsToTest QC.Laws {..} = TestList $ uncurry mkTest <$> lawsProperties
  where
    mkTest name = propertyToTest (lawsTypeclass <> "." <> name <> " satisfied")

filterLaws :: (String -> Bool) -> QC.Laws -> QC.Laws
filterLaws p QC.Laws {..} = QC.Laws { lawsProperties = filter (p . fst) lawsProperties, .. }

deriving newtype instance Arbitrary a => Arbitrary (Id a)
deriving newtype instance Arbitrary (f a) => Arbitrary (Done f a)
deriving newtype instance Arbitrary (f a) => Arbitrary (Todo f a)
deriving newtype instance Arbitrary (f a) => Arbitrary (Dispatcher l f a)
