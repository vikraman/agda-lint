module Bad where

open import B
open import A

f : Set -> Set
f = \x -> let y = x in y 
g' : Set
g' = ⊥-elim
h = a \/ b \\ c
k = \_ -> \(x : A) -> x
