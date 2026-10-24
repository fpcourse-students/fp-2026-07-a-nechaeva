module Test.Prelude
  ( module Control.DeepSeq
  , module Data.Coerce
  , module Data.Proxy
  , module Test.HUnit
  , module Test.Lazy
  , module Test.Properties
  , module Test.QuickCheck
  , module Test.QuickCheck.Arbitrary.Generic
  , module Test.QuickCheck.Classes.Base
  , module Test.Run
  , module Test.Utils
  , module MetaUtils
  ) where

import Control.DeepSeq
import Data.Coerce (Coercible)
import Data.Proxy
import Test.HUnit (Test (..), assertFailure, assertEqual, assertBool)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Classes.Base
import Test.Run
import Test.Lazy
import Test.Properties
import Test.Utils
import MetaUtils
