module Lib.Syntax where

variable
  A B C : Set

postulate
  Val : Set → Set

syntax Val X = ⟦ X ⟧
