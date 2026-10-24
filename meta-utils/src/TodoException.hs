module TodoException
  ( TodoException (..)
  , Todoable (..)
  , todo'
  ) where

import Control.Exception
import Data.Int
import Data.Proxy
import Data.Set qualified as Set
import Data.Typeable
import GHC.TypeLits

newtype TodoException = TodoException String
instance Exception TodoException
instance Show TodoException where
  show (TodoException msg) = "Not implemented: " <> msg

class Todoable s where
  todo :: s -> forall a. a

instance Todoable String where
  todo name = throw $ TodoException name

instance (KnownSymbol funcName, Typeable constraint, Typeable ty)
  => Todoable (Proxy '(funcName :: Symbol, constraint, ty)) where
  todo Proxy = todo $ typeName @constraint <> "." <> symbolVal (Proxy @funcName) <>
    if typeName @ty `Set.member` ignore then "" else " for " <> typeName @ty
    where
      -- Sometimes not implemented dispatch happens for standard classes, do not confuse users in that case.
      ignore = Set.fromList [typeName @Bool, typeName @Int, typeName @Int8, typeName @Int64, typeName @String]

typeName :: forall a. Typeable a => String
typeName = tyConName $ typeRepTyCon $ typeRep $ Proxy @a

todo' :: forall (s :: Symbol) c a. (KnownSymbol s, Typeable c, Typeable a) => forall b. b
todo' = todo $ Proxy @'(s, c, a)
