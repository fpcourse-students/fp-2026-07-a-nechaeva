-- | Генерация placeholder-инстансов.
module TodoMacro
  ( -- reexports support generated code
    module Control.Exception
  , module TodoException
  , todoImpl
  ) where

import Control.Exception (throw)
import Control.Monad
import Language.Haskell.TH
import TodoException (TodoException (..))

type DataName = String
type FunctionName = String
type ClassName = String

data EntryInfo = EntryInfo
  { entryDataName :: DataName
  , entryClassName :: ClassName
  , entryFunctionName :: FunctionName
  }

mkTodoString :: EntryInfo -> String
mkTodoString EntryInfo {..} = entryClassName <> "." <> entryFunctionName <> " for " <> entryDataName

mkVarE :: String -> Exp
mkVarE = VarE . mkName

todoE :: EntryInfo -> Exp
todoE info = AppE (mkVarE "todo") (LitE $ StringL $ mkTodoString info)

todoB :: EntryInfo -> Body
todoB = NormalB . todoE

nKindArgs :: Kind -> Int
nKindArgs StarT = 1
nKindArgs (AppT _ rest) = 1 + nKindArgs rest
nKindArgs _ = error "Unexpected kind form"

isSigD :: Dec -> Bool
isSigD = \case
  SigD _ _ -> True
  _ -> False

sigToDec :: DataName -> ClassName -> Dec -> Dec
sigToDec dn cn (SigD nm _) = FunD nm [Clause [] (todoB (EntryInfo dn cn (nameBase nm))) []]
sigToDec _ _ _ = error "only SigD expected!"

data ClassInfo = ClassInfo { className :: Name, classNParams :: Int, classNTargetTyArgs :: Int }
data DataInfo = DataInfo { dataName :: Name, dataNTyArgs :: Int }

mkInstanceTarget :: ClassInfo -> DataInfo -> Q Type
mkInstanceTarget ClassInfo{..} DataInfo{..} = do
  let nSpecializeVars = dataNTyArgs - classNTargetTyArgs + 1
  vars <- replicateM nSpecializeVars (VarT <$> newName "a")
  let specializedData = foldl AppT (ConT dataName) vars
  -- Handle first vars for multiparametric type classes a la MonadError.
  -- Tries to instantiate params with first specialized variables.
  -- E.g. instance MonadError e (ExceptT e m).
  let otherVars = take (classNParams - 1) vars
  pure $ foldl AppT (ConT className) (otherVars ++ [specializedData])

todoImpl :: Name -> Name -> Q [Dec]
todoImpl className dataName = do
  (classInfo, sigs) <- reify className >>= \case
    ClassI (ClassD _ _ tyParams _ sigs) _ | KindedTV _ _ kind <- last tyParams -> do
      let classNParams = length tyParams
      let classNTargetTyArgs = nKindArgs kind
      pure (ClassInfo {..}, sigs)
    x -> fail $ "expected type class, got " <> show x
  dataNTyArgs <- reify dataName >>= \case
    TyConI (DataD _ _ tyArgs _ _ _) -> pure $ length tyArgs
    TyConI (NewtypeD _ _ tyArgs _ _ _) -> pure $ length tyArgs
    TyConI (TySynD (nameBase -> "->") [] _) -> pure 2
    TyConI (TySynD name [] _) | name == ''String -> pure 0 -- module of String differs across GHCs
    x -> fail $ "unsupported data, got " <> show x
  target <- mkInstanceTarget classInfo DataInfo {..}
  let decls = map (sigToDec (show dataName) (show className)) $ filter isSigD sigs
  pure [InstanceD Nothing [] target decls]
