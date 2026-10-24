-- | Lambda calculus homework checker: parse, check, reduce, type.
module Lambda
  ( -- * Syntax
    Name
  , SrcPos (..)
  , prettyPos
  , Type (..)
  , Expr (..)
  , Language (..)
  , Stmt (..)
  , Program (..)
  , lam
  , apps
    -- * Parse
  , parseProgram
  , parseExpr
  , parseType
  , churchNumeral
  , desugarNumerals
    -- * Print
  , prettyExpr
  , prettyFocus
  , prettyCaret
  , prettyType
  , prettyTypeGreek
  , prettyDecoded
    -- * Explain
  , ExplainView (..)
  , TreePiece (..)
  , maxBinderStyles
  , explain
  , renderExplain
    -- * Evaluate
  , Ctx (..)
  , pureCtx
  , Strategy (..)
  , Path (..)
  , EvalError (..)
  , RedexKind (..)
  , Step (..)
  , defaultLimit
  , followSteps
  , findRedex
  , stepBeta
  , normalForm
  , normalFormWith
  , betaPaths
  , contractBeta
  , unfoldings
  , namedUnfoldings
  , unfoldingsOf
  , reachable
  , Verdict (..)
  , betaEq
  , isWhnf
  , isHnf
  , isNf
  , normalizing
    -- * Substitution and equality
  , freeVars
  , subst
  , alphaEq
  , expandAll
  , etaReduce
  , erase
    -- * Types
  , TypeError (..)
  , inferType
  , checkChurch
  , typesEqualUpToRenaming
  , isInstanceOf
    -- * Check / load
  , CheckResult (..)
  , TaskResult (..)
  , TaskStatus (..)
  , Loaded (..)
  , Session (..)
  , loadFile
  , loadSession
  , checkFile
  , checkLoaded
  , taskStatus
  , prettyResults
  , reportOk
  , inhabitable
    -- * REPL
  , ReplCmd (..)
  , CommandResult (..)
  , parseCommand
  , runLine
  , runInput
  , execCommand
  , renderResult
  ) where

import Lambda.Check
import Lambda.Command
import Lambda.Decode
import Lambda.Eval
import Lambda.Explain
import Lambda.Parser
import Lambda.Pretty
import Lambda.Subst (alphaEq, erase, etaReduce, expandAll, freeVars, subst)
import Lambda.Syntax
import Lambda.Types
