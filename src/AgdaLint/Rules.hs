-- built-in rules
module AgdaLint.Rules (builtinRules) where

import           AgdaLint.Rule
import           AgdaLint.Rules.Naming
import           AgdaLint.Rules.Style

builtinRules :: [Rule]
builtinRules = namingRules ++ styleRules
