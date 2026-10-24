-- | Abstract syntax: terms, types, statements of a @.lam@ file.
module Lambda.Syntax
  ( Name
  , SrcPos (..)
  , prettyPos
  , Type (..)
  , Expr (..)
  , Language (..)
  , Arrow (..)
  , arrowNames
  , ChainOpts (..)
  , defaultChainOpts
  , ChainStrategy (..)
  , Predicate (..)
  , Stmt (..)
  , Program (..)
  , lam
  , apps
  ) where

-- | Surface names (variables, binders, definitions).
type Name = String

-- | A source location used in error messages.
data SrcPos = SrcPos
  { posFile :: FilePath
  , posLine :: Int
  , posCol  :: Int
  } deriving (Eq, Ord, Show)

prettyPos :: SrcPos -> String
prettyPos (SrcPos file line col) =
  file ++ ":" ++ show line ++ ":" ++ show col


-- | Simple types: variables, constants (@Int@, @Bool@), arrows.
data Type
  = TVar Name
  | TCon Name
  | TArr Type Type
  deriving (Eq, Ord, Show)

-- | Terms.
--
-- @Lam x ann body@ is @\\x. body@, optionally annotated @\\x:T. body@
-- (Church style). Multi-argument lambdas are desugared to nested 'Lam'.
-- @Lit n@ is an integer constant (in @pure@ files the parser turns digits
-- into Church numerals instead, so 'Lit' appears only in @typed@ files).
-- @Subst [(x, n), ...] m@ is the meta-level simultaneous substitution
-- @[x |-> n, ...] m@; it is only allowed as the first line of a chain.
data Expr
  = Var Name
  | Lam Name (Maybe Type) Expr
  | App Expr Expr
  | Lit Integer
  | Hole SrcPos
  | Subst [(Name, Expr)] Expr
  deriving (Eq, Show)

lam :: Name -> Expr -> Expr
lam x = Lam x Nothing

apps :: Expr -> [Expr] -> Expr
apps = foldl App

data Language = Pure | Typed
  deriving (Eq, Show)

-- | Arrows between chain lines.
data Arrow
  = ArrBeta        -- ^ @~b~>@ one β-step
  | ArrDelta Name  -- ^ @~K~>@ one occurrence of the name @K@ unfolded (or folded)
  | ArrAlpha       -- ^ @=a=@ α-renaming
  | ArrMany        -- ^ @~~>@ at most k steps
  | ArrSubst       -- ^ @~s~>@ substitution performed
  | ArrEta         -- ^ @~eta~>@ one η-step: @\\x. f x@ to @f@, or back
  deriving (Eq, Show)

-- | Names that cannot be defined because @~b~>@, @~s~>@ and @~eta~>@ are
-- arrows. The η-arrow is spelled out: a one-letter @e@ is a name students
-- are likely to use for their own definitions.
arrowNames :: [Name]
arrowNames = ["b", "s", "eta"]

data ChainStrategy = ChainNormal | ChainApplicative
  deriving (Eq, Show)

data ChainOpts = ChainOpts
  { chainFrom     :: Maybe Expr
  , chainTo       :: Maybe Expr
  , chainSteps    :: Int
  , chainStrategy :: Maybe ChainStrategy
  } deriving (Eq, Show)

defaultChainOpts :: ChainOpts
defaultChainOpts = ChainOpts Nothing Nothing 3 Nothing

data Predicate
  = PClosed
  | PWhnf
  | PHnf
  | PNf
  | PNormalizing
  | PUses Name
  | PAvoids Name
  | PNot Predicate
  deriving (Eq, Show)

-- | One line (or, for chains, one block) of a @.lam@ file.
data Stmt
  = SLanguage SrcPos Language
  | SImport SrcPos FilePath
  | SVars SrcPos [Name]
  | STask SrcPos String String              -- ^ id, free text
  | SRule SrcPos String                     -- ^ kept for the reader, ignored
  | SDef SrcPos Name Expr
  | SExpect SrcPos Expr Expr
  | SNormal SrcPos Expr Expr                -- ^ @nf A = B@: like @expect@, and @B@ is written as a value
  | SFree SrcPos Name [Name]
  | SRename SrcPos Name Expr
  | SMinimal SrcPos Name Expr String        -- ^ the raw text, for the parentheses count
  | SCheck SrcPos Name [Predicate]
  | SChain SrcPos Name ChainOpts [(Maybe Arrow, Expr)]
  | SType SrcPos Name (Maybe Type)          -- ^ Nothing = @none@
  | SChurch SrcPos Name Expr
  | SInhabit SrcPos Type (Maybe Int) [Name]  -- ^ @inhabit T (k): names@ — at least @k@ inhabitants
  | SFamily SrcPos Type Expr
  deriving (Eq, Show)

newtype Program = Program [Stmt]
  deriving (Eq, Show)
