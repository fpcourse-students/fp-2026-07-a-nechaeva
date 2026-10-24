{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | Legacy placeholder-mechanism for type classes.
-- Оставь надежду всяк сюда входящий.
module ClassDispatcher
  ( (:>), Result1, Result2, Dispatcher (..)
  , wrap1, wrap2, unwrap1, unwrap2
  , Id (..)
  , Done, DoneSelector (..)
  , Todo, TodoSelector (..)
  , Selector, F, TLookupC1, TLookupC2
  ) where

import Control.Applicative
import Data.Coerce
import Data.Foldable
import Data.Functor.Identity
import Data.Kind
import Data.Typeable
import GHC.Generics (Generic)
import TodoException

type (key :: k) :> (value :: k') = '(key, value)
type TMap k v = [(k, v)]
type family TLookupOrDefault (key :: k) (d :: v) (map :: TMap k v) where
  TLookupOrDefault _ d '[] = d
  TLookupOrDefault key _ ('(key, value) : _) = value
  TLookupOrDefault key d ('(key', _) : map) = TLookupOrDefault key d map


-- Data.Functor.Identity has stock strategy of deriving Show.
newtype Id a = Id a
  deriving newtype (Show, Read, Eq, Ord, Enum, Bounded, Num, Semigroup, Monoid)
  deriving stock (Generic, Functor)
  deriving Applicative via Identity


type F = Type -> Type
type Selector = F -> Type -> Type
type Results k = TMap k Selector

newtype Dispatcher (l :: Results k) (f :: F) a = Dispatcher (f a)
type Result1 (l :: Results k) = Dispatcher l Id
type Result2 (l :: Results k) = Dispatcher l

wrap1 :: forall d l a. (d ~ Dispatcher l Id) => a -> d a
wrap1 = Dispatcher . Id

wrap2 :: forall d l f a. (d ~ Dispatcher l f) => f a -> d a
wrap2 = Dispatcher

unwrap1 :: Dispatcher l Id a -> a
unwrap1 (Dispatcher (Id x)) = x

unwrap2 :: Dispatcher l f a -> f a
unwrap2 (Dispatcher x) = x


type Class1 = Type -> Constraint
type Class2 = F -> Constraint

type TLookup2To1C (cls :: Class1) (l :: Results Class2) (f :: F) (a :: Type) = (cls a, cls (f a))
deriving newtype instance TLookup2To1C Show l f a => Show (Dispatcher l f a)
deriving newtype instance TLookup2To1C Eq l f a => Eq (Dispatcher l f a)

type TLookup1To2C (cls :: Class2) (l :: Results Class1) (f :: F) = cls f
deriving newtype instance TLookup1To2C Functor l f => Functor (Dispatcher l f)
deriving newtype instance TLookup1To2C Applicative l f => Applicative (Dispatcher l f)
deriving newtype instance TLookup1To2C Alternative l f => Alternative (Dispatcher l f)
deriving newtype instance TLookup1To2C Monad l f => Monad (Dispatcher l f)
deriving newtype instance TLookup1To2C Foldable l f => Foldable (Dispatcher l f)

type TLookup (key :: k) (l :: Results k) = TLookupOrDefault key Done l
type TLookupC1 (cls :: Class1) (l :: Results Class1) (s :: Selector) (f :: F) (a :: Type) =
  (s ~ TLookup cls l, Coercible (f a) (s f a), cls (s f a))
type TLookupC2 (cls :: Class2) (l :: Results Class2) (s :: Selector) (f :: F) =
  (s ~ TLookup cls l, forall a. Coercible (f a) (s f a), cls (s f))

deriving via forall (s :: Selector) (f :: F) a. s f a instance TLookupC1 Show l s f a => Show (Dispatcher l f a)
deriving via forall (s :: Selector) (f :: F) a. s f a instance TLookupC1 Read l s f a => Read (Dispatcher l f a)
deriving via forall (s :: Selector) (f :: F) a. s f a instance TLookupC1 Eq l s f a => Eq (Dispatcher l f a)
deriving via forall (s :: Selector) (f :: F) a. s f a instance (TLookupC1 Eq l s' f a, TLookupC1 Ord l s f a) => Ord (Dispatcher l f a)
deriving via forall (s :: Selector) (f :: F) a. s f a instance TLookupC1 Enum l s f a => Enum (Dispatcher l f a)
deriving via forall (s :: Selector) (f :: F) a. s f a instance TLookupC1 Bounded l s f a => Bounded (Dispatcher l f a)
deriving via forall (s :: Selector) (f :: F) a. s f a instance TLookupC1 Num l s f a => Num (Dispatcher l f a)
deriving via forall (s :: Selector) (f :: F) a. s f a instance TLookupC1 Semigroup l s f a => Semigroup (Dispatcher l f a)
deriving via forall (s :: Selector) (f :: F) a. s f a instance (TLookupC1 Semigroup l s' f a, TLookupC1 Monoid l s f a) => Monoid (Dispatcher l f a)
deriving via forall (s :: Selector) (f :: F). s f instance TLookupC2 Functor l s f => Functor (Dispatcher l f)
deriving via forall (s :: Selector) (f :: F). s f instance (TLookupC2 Functor l s' f, TLookupC2 Applicative l s f) => Applicative (Dispatcher l f)
deriving via forall (s :: Selector) (f :: F). s f instance (TLookupC2 Functor l s' f, TLookupC2 Applicative l s'' f, TLookupC2 Alternative l s f) => Alternative (Dispatcher l f)
deriving via forall (s :: Selector) (f :: F). s f instance (TLookupC2 Functor l s' f, TLookupC2 Applicative l s'' f, TLookupC2 Monad l s f) => Monad (Dispatcher l f)
deriving via forall (s :: Selector) (f :: F). s f instance TLookupC2 Foldable l s f => Foldable (Dispatcher l f)


type Done = DoneSelector

newtype DoneSelector (f :: F) a = DoneSelector (f a)
  deriving newtype (Show, Read)
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Semigroup, Monoid)
  deriving stock Generic

deriving newtype instance Functor f => Functor (DoneSelector f)
deriving newtype instance Applicative f => Applicative (DoneSelector f)
deriving newtype instance Alternative f => Alternative (DoneSelector f)
deriving newtype instance Monad f => Monad (DoneSelector f)
deriving newtype instance Foldable f => Foldable (DoneSelector f)


type Todo = TodoSelector

newtype TodoSelector (f :: F) a = TodoSelector (f a)
  deriving stock Generic

instance Typeable a => Show (TodoSelector f a) where
  show =  todo' @"show" @Show @a
  showsPrec = todo' @"showsPrec" @Show @a
  showList = todo' @"showList" @Show @a

instance Typeable a => Read (TodoSelector f a) where
  readsPrec = todo' @"readsPrec" @Read @a
  readList = todo' @"readList" @Read @a

instance Typeable a => Eq (TodoSelector f a) where
  (==) = todo' @"(==)" @Eq @a
  (/=) = todo' @"(/=)" @Eq @a

instance Typeable a => Ord (TodoSelector f a) where
  (<) = todo' @"(<)" @Ord @a
  (<=) = todo' @"(<=)" @Ord @a
  (>) = todo' @"(>)" @Ord @a
  (>=) = todo' @"(>=)" @Ord @a
  max = todo' @"(max)" @Ord @a
  min = todo' @"(min)" @Ord @a

instance Typeable a => Enum (TodoSelector f a) where
  succ = todo' @"succ" @Enum @a
  pred = todo' @"pred" @Enum @a
  toEnum = todo' @"toEnum" @Enum @a
  fromEnum = todo' @"fromEnum" @Enum @a
  enumFrom = todo' @"enumFrom" @Enum @a
  enumFromThen = todo' @"enumFromThen" @Enum @a
  enumFromTo = todo' @"enumFromTo" @Enum @a
  enumFromThenTo = todo' @"enumFromThenTo" @Enum @a

instance Typeable a => Bounded (TodoSelector f a) where
  minBound = todo' @"minBound" @Bounded @a
  maxBound = todo' @"maxBound" @Bounded @a

instance Typeable a => Num (TodoSelector f a) where
  (+) = todo' @"(+)" @Num @a
  (-) = todo' @"(-)" @Num @a
  (*) = todo' @"(*)" @Num @a
  negate = todo' @"negate" @Num @a
  abs = todo' @"abs" @Num @a
  signum = todo' @"signum" @Num @a
  fromInteger = todo' @"fromInteger" @Num @a

instance Typeable a => Semigroup (TodoSelector f a) where
  (<>) = todo' @"(<>)" @Semigroup @a

instance Typeable a => Monoid (TodoSelector f a) where
  mempty = todo' @"mempty" @Monoid @a

instance Typeable f => Functor (TodoSelector f) where
  fmap = todo' @"fmap" @Functor @f
  (<$) = todo' @"(<$)" @Functor @f

instance Typeable f => Applicative (TodoSelector f) where
  pure = todo' @"pure" @Applicative @f
  (<*>) = todo' @"(<*>)" @Applicative @f
  liftA2 = todo' @"liftA2" @Applicative @f

instance Typeable f => Alternative (TodoSelector f) where
  empty = todo' @"empty" @Alternative @f
  (<|>) = todo' @"(<|>)" @Alternative @f
  some = todo' @"some" @Alternative @f
  many = todo' @"many" @Alternative @f

instance Typeable f => Monad (TodoSelector f) where
  (>>=) = todo' @"(>>=)" @Monad @f

instance Typeable f => Foldable (TodoSelector f) where
  fold = todo' @"fold" @Foldable @f
  foldMap = todo' @"foldMap" @Foldable @f
  foldMap' = todo' @"foldMap'" @Foldable @f
  foldr = todo' @"foldr" @Foldable @f
  foldr' = todo' @"foldr'" @Foldable @f
  foldl = todo' @"foldl" @Foldable @f
  foldl' = todo' @"foldl'" @Foldable @f
  foldr1 = todo' @"foldr1" @Foldable @f
  foldl1 = todo' @"foldl1" @Foldable @f
  toList = todo' @"toList" @Foldable @f
  null = todo' @"null" @Foldable @f
  length = todo' @"length" @Foldable @f
  elem = todo' @"elem" @Foldable @f
  maximum = todo' @"maximum" @Foldable @f
  minimum = todo' @"minimum" @Foldable @f
  sum = todo' @"sum" @Foldable @f
  product = todo' @"product" @Foldable @f
