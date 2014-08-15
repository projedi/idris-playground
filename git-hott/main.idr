module Main

import Control.Category
import Control.Isomorphism

instance Category Iso where
  id = isoRefl
  i1 . i2 = isoTrans i2 i1

swapAt : Char -> Char -> Fin n -> Iso (Vect n Char) (Vect n Char)
swapAt a b i = ?swapAt
  {-
  MkIso f f prf prf
 where f = updateAt i g
       g c = if c == a then b else if c == b then a else c
       prf = ?swapAt_prf
       -}

data Patch : {n : Nat} -> Type where
  pId : Patch
  pCompose : Patch {n} -> Patch {n} -> Patch {n}
  pInvert : Patch {n} -> Patch {n}
  pSwapAt : Char -> Char -> Fin n -> Patch {n}

interp : Patch {n} -> Iso (Vect n Char) (Vect n Char)
interp pId = id
interp (pCompose q p) = interp q . interp p
interp (pInvert p) = isoSym (interp p)
interp (pSwapAt a b i) = swapAt a b i

merge : (p : Patch) -> (q : Patch) -> (q' : Patch ** (p' : Patch ** Maybe (pCompose q' p = pCompose p' q)))
merge = ?merge
