-- agda style linter, for writing rules
module AgdaLint
  ( -- entry point
    defaultMain
  , builtinRules
    -- rules
  , Rule (..)
  , Check
  , Settings
  , rule
  , ruleWith
  , core
    -- findings and fixes
  , Level (..)
  , Pos (..)
  , Finding (..)
  , Fix (..)
  , finding
  , fixed
  , replaceTok
    -- sources
  , Context (..)
  , SourceFile (..)
  , Line (..)
  , LineRole (..)
  , Token (..)
  , TokenKind (..)
  , Clause (..)
  , Binder (..)
  , VariableDecl (..)
  , Notation (..)
  , OpenDecl (..)
  , ImportModifier (..)
    -- lines and tokens
  , codeLines
  , blockLines
  , clauseLines
  , codeTokens
  , wordsOf
  , firstText
  , indentOf
  , lineStart
  ) where

import           AgdaLint.Lexer
import           AgdaLint.Main
import           AgdaLint.Project
import           AgdaLint.Rule
import           AgdaLint.Rules
import           AgdaLint.Source
import           AgdaLint.Types
