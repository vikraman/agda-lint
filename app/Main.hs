module Main (main) where

import           AgdaLint

main :: IO ()
main = defaultMain builtinRules
