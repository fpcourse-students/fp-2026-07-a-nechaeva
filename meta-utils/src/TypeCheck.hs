{- |
Проверки задач, где ответ студента — тип. Неверный ответ не ломает сборку тестов, а сообщение
о провале не печатает ожидаемый ответ.

Два способа сравнить типы:

* 'typeIs' — через @Typeable@: оба типа превращаются в 'SomeTypeRep' во время выполнения.
  Годится для типов без переменных.
* форма типа — строка, в которую Template Haskell рендерит дерево типа: 'shapeOfType' для
  сигнатуры именованного значения (или кайнда типа), 'synonymShape' для синонима типа.
  GHC при этом не проверяет, подходит ли тип какому-либо выражению: сравнивается запись.

Что форма считает одинаковым:

* имена переменных: @forall c. (a -> c) -> c@ и @forall r. (a -> r) -> r@;
* порядок переменных под одним @forall@: @forall a b. a -> b -> a@ и @forall b a. a -> b -> a@
  (переменные нумеруются по первому вхождению в тип, а не по порядку связывания);
* лишние скобки, @String@ и @[Char]@, аннотацию @(c :: Type)@ и её отсутствие;
* порядок ограничений в контексте.

Что форма различает: порядок аргументов функции, число и порядок параметров синонима
(параметры нумеруются по порядку объявления), вложенность кванторов.

Заглушка для типов — 'Todo': задача, в которой ответ всё ещё 'Todo', получает статус TODO,
а не FAILED.
-}
module TypeCheck
  ( Todo
  , typeIs
  , shapeOfType, shapeOfKind, synonymShape
  , assertShape, assertShapeMarked, isTodo
  ) where

import Control.Monad (when)
import Data.Char (isDigit)
import Data.List qualified as List
import Language.Haskell.TH
import Test.HUnit (Test (..), assertBool)
import TodoException (todo)
import Type.Reflection (SomeTypeRep (..), Typeable, typeRep)

-- | Заглушка для типов: студент заменяет её своим типом.
data Todo

-- | Тип @actual@ (ответ студента) совпадает с @expected@. Заглушка 'Todo' — задача не начата.
typeIs :: forall {k1} {k2} (expected :: k1) (actual :: k2). (Typeable expected, Typeable actual) => String -> Test
typeIs what = TestCase do
  let actual = SomeTypeRep (typeRep @actual)
  when (actual == SomeTypeRep (typeRep @Todo)) $ todo what
  assertBool (what <> ": тип " <> show actual <> " не тот, что требуется") $
    actual == SomeTypeRep (typeRep @expected)

-- | Сплайс: форма типа именованного значения. Для значения с сигнатурой это сигнатура,
-- для значения без сигнатуры — тип, который вывел GHC.
shapeOfType :: Name -> Q Exp
shapeOfType name = reifyType name >>= stringE . render []

-- | Сплайс: форма кайнда типа (для синонима — его кайнд целиком, включая параметры).
shapeOfKind :: Name -> Q Exp
shapeOfKind = shapeOfType

-- | Сплайс: форма синонима типа — число параметров и правая часть, например
-- @"2 (forall v3 (-> (-> v1 (-> v2 v3)) v3))"@.
synonymShape :: Name -> Q Exp
synonymShape name = reify name >>= \case
  TyConI (TySynD _ params rhs) ->
    stringE $ show (length params) <> " " <> render (map binderName params) rhs
  _ -> stringE "<not a type synonym>"

-- | Форма — заглушка 'Todo' (у синонима — с любым числом параметров).
isTodo :: String -> Bool
isTodo shape = case words shape of
  ["Todo"] -> True
  [arity, "Todo"] -> all isDigit arity
  _ -> False

-- | Форма совпадает с ожидаемой; заглушка — задача не начата.
assertShape :: String -> String -> String -> Test
assertShape what expected actual = assertShapeMarked what actual expected actual

-- | То же, но «начата ли задача» определяется по отдельной форме-маркеру (например, по правой
-- части синонима, когда сравнивается его кайнд).
assertShapeMarked :: String -> String -> String -> String -> Test
assertShapeMarked what marker expected actual = TestCase do
  when (isTodo marker) $ todo what
  assertBool (what <> ": тип не тот, что требуется") $ actual == expected

binderName :: TyVarBndr flag -> Name
binderName = \case
  PlainTV n _ -> n
  KindedTV n _ _ -> n

binderKind :: TyVarBndr flag -> Maybe Kind
binderKind = \case
  PlainTV _ _ -> Nothing
  KindedTV _ _ StarT -> Nothing
  KindedTV _ _ k -> Just k

-- | Рендер дерева типа: аппликация — @(f x y)@, стрелка — @(-> a b)@, квантор —
-- @(forall v1 v2 t)@, контекст — @(=> (C v1) t)@; имена без модулей, продвинутость
-- конструкторов и явные аппликации кайндов не различаются.
--
-- Нумерация переменных: сначала параметры синонима в порядке объявления, затем переменные
-- под кванторами в порядке первого вхождения в тип, затем ни разу не использованные.
render :: [Name] -> Type -> String
render params ty = go ty
  where
    bound = binders ty
    numbered = params ++ List.nub (filter (`elem` bound) (occurrences ty) ++ bound)
    variable n = maybe (nameBase n) (\i -> "v" <> show (i + 1)) $ List.elemIndex n numbered

    go = \case
      ForallT bs ctx body -> forall' bs ctx body
      ForallVisT bs body -> forall' bs [] body
      AppT ListT (ConT c) | nameBase c == "Char" -> "String"
      t@AppT {} -> let (h, args) = spine t in "(" <> unwords (go h : map go args) <> ")"
      AppKindT t _ -> go t
      SigT t _ -> go t
      ParensT t -> go t
      InfixT l n r -> go $ AppT (AppT (ConT n) l) r
      UInfixT l n r -> go $ AppT (AppT (ConT n) l) r
      PromotedInfixT l n r -> go $ AppT (AppT (ConT n) l) r
      PromotedUInfixT l n r -> go $ AppT (AppT (ConT n) l) r
      VarT n -> variable n
      ConT n | nameBase n == "String" -> "String"
      ConT n -> nameBase n
      PromotedT n -> nameBase n
      TupleT 0 -> "()"
      TupleT n -> "(" <> replicate (n - 1) ',' <> ")"
      PromotedTupleT n -> "(" <> replicate (n - 1) ',' <> ")"
      ArrowT -> "->"
      MulArrowT -> "->"
      ListT -> "[]"
      PromotedNilT -> "[]"
      PromotedConsT -> ":"
      StarT -> "Type"
      ConstraintT -> "Constraint"
      LitT (NumTyLit n) -> show n
      LitT (StrTyLit s) -> show s
      LitT (CharTyLit c) -> show c
      EqualityT -> "~"
      WildCardT -> "_"
      ImplicitParamT n t -> "(?" <> n <> " " <> go t <> ")"
      UnboxedTupleT n -> "(#" <> replicate (n - 1) ',' <> "#)"
      UnboxedSumT n -> "(#" <> replicate (n - 1) '|' <> "#)"

    forall' :: [TyVarBndr flag] -> Cxt -> Type -> String
    forall' bs ctx body =
      let shown = map snd $ List.sortOn fst [(List.elemIndex (binderName b) numbered, binder b) | b <- bs]
          inner = if null ctx then go body else "(=> " <> unwords (List.sort $ map go ctx) <> " " <> go body <> ")"
      in if null bs then inner else "(forall " <> unwords shown <> " " <> inner <> ")"

    binder :: TyVarBndr flag -> String
    binder b = case binderKind b of
      Nothing -> variable (binderName b)
      Just k -> "(" <> variable (binderName b) <> " :: " <> go k <> ")"

-- | Переменные, связанные кванторами, в порядке связывания.
binders :: Type -> [Name]
binders = \case
  ForallT bs ctx body -> map binderName bs ++ concatMap (foldMap binders . binderKind) bs
    ++ concatMap binders ctx ++ binders body
  ForallVisT bs body -> map binderName bs ++ binders body
  AppT f x -> binders f ++ binders x
  AppKindT t _ -> binders t
  SigT t _ -> binders t
  ParensT t -> binders t
  InfixT l _ r -> binders l ++ binders r
  UInfixT l _ r -> binders l ++ binders r
  PromotedInfixT l _ r -> binders l ++ binders r
  PromotedUInfixT l _ r -> binders l ++ binders r
  ImplicitParamT _ t -> binders t
  _ -> []

-- | Вхождения переменных слева направо: сначала тело квантора, потом его контекст
-- и кайнды связываемых переменных.
occurrences :: Type -> [Name]
occurrences = \case
  ForallT bs ctx body -> occurrences body ++ concatMap occurrences ctx
    ++ concatMap (foldMap occurrences . binderKind) bs
  ForallVisT bs body -> occurrences body ++ concatMap (foldMap occurrences . binderKind) bs
  AppT f x -> occurrences f ++ occurrences x
  AppKindT t _ -> occurrences t
  SigT t _ -> occurrences t
  ParensT t -> occurrences t
  InfixT l _ r -> occurrences l ++ occurrences r
  UInfixT l _ r -> occurrences l ++ occurrences r
  PromotedInfixT l _ r -> occurrences l ++ occurrences r
  PromotedUInfixT l _ r -> occurrences l ++ occurrences r
  ImplicitParamT _ t -> occurrences t
  VarT n -> [n]
  _ -> []

-- | Голова аппликации и её аргументы: @f x y@ → @(f, [x, y])@.
spine :: Type -> (Type, [Type])
spine = \case
  AppT f x -> let (h, args) = spine f in (h, args ++ [x])
  t -> (t, [])
