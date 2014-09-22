module elem

using (xs : List a)
  data Elem : a -> List a -> Type where
    Here : Elem x (x :: xs)
    There : Elem x xs -> Elem x (y :: xs)

instance Uninhabited (Elem x []) where
  uninhabited Here impossible
  uninhabited (There _) impossible

isElem : DecEq a => (x : a) -> (xs : List a) -> Dec (Elem x xs)
isElem x [] = No absurd
isElem x (y :: xs) with (decEq x y)
  isElem x (x :: xs) | (Yes refl) = Yes Here
  isElem x (y :: xs) | (No contra) with (isElem x xs)
    isElem x (y :: xs) | (No contra) | (Yes prf) = Yes (There prf)
    isElem x (y :: xs) | (No contra) | (No f) = No go
     where go Here impossible
           go (There z) = f z
