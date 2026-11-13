module More where

open import Data.Empty renaming (⊥ to Empty) using (⊥-elim)

mutual
  f : Set
    → Set
  f x with x
  ...    | y = ⊥-elim y

proof =
  begin
    x
