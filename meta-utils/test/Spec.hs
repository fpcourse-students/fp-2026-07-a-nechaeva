import Control.Exception
import Data.Functor.Identity
import MetaUtils
import Test.HUnit (Test (..), assertFailure, assertEqual)
import Test.HUnit qualified as HU
import TypeCheckSpec qualified
import PublishSpec qualified

data Data a = Ok !a | NotOk deriving Show
data Data' f a = Ok' !(f a) | NotOk' deriving Show
data Data'' e a = Ok'' !a | NotOk'' !e deriving Show
data ExceptT e (m :: Type -> Type) a

class MonadTrans t where
  lift :: m a -> t m a

class MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

class Expr domain where
  c :: Int -> domain

todoImpl ''Eq ''Data
todoImpl ''Functor ''Data
todoImpl ''Applicative ''Data
todoImpl ''Monad ''Data
todoImpl ''Foldable ''Data
todoImpl ''Traversable ''Data
todoImpl ''MonadTrans ''Data'
todoImpl ''MonadError ''Data''
todoImpl ''MonadError ''ExceptT
todoImpl ''Expr ''String

main :: IO ()
main = HU.runTestTTAndExit $ TestList
  [ testPP
  , testMacro
  , TypeCheckSpec.tests
  , PublishSpec.tests
  ]

testPP :: Test
testPP = TestCase $ assertEqual "out show" "Ok 42" $ pretty $ OutShow $ Ok (42 :: Int)

testMacro :: Test
testMacro = TestList
  [ TestCase $ assertThrows $ value == value
  , TestCase $ assertThrows $ value /= value
  , TestCase $ assertThrows $ fmap (+1) value
  , TestCase $ assertThrows $ value >>= \x -> value >>= \y -> pure (x + y)
  , TestCase $ assertThrows (lift (Identity 42) :: Data' Identity Int)
  , TestCase $ assertThrows (throwError "aaaa" :: Data'' String Int)
  , TestCase $ assertThrows (catchError undefined undefined :: ExceptT String Identity Int)
  , TestCase $ assertThrows $ c 42 == "42"
  ]
  where
    value = Ok 42 :: Data Int

    assertThrows :: a -> IO ()
    assertThrows comp = try (evaluate comp) >>= \case
      Left e -> case fromException @TodoException e of
        Just _ -> pure ()
        Nothing -> throw e
      Right _ -> assertFailure "Expected todo exception, got result :("
