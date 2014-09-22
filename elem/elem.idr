module elem

using (xs : List a)
  data Elem : a -> List a -> Type where
    Here : Elem x (x :: xs)
    There : Elem x xs -> Elem x (y :: xs)

instance Uninhabited (Elem x []) where
  uninhabited Here impossible
  uninhabited (There _) impossible

isElem : DecEq a => (x : a) -> (xs : List a) -> Either (Elem x xs) (Elem x xs -> _|_)
isElem x [] = Right absurd
isElem x (y :: xs) with (decEq x y)
  isElem x (x :: xs) | (Yes refl) = Left Here
  isElem x (y :: xs) | (No contra) with (isElem x xs)
    isElem x (y :: xs) | (No contra) | (Left z) = Left (There z)
    isElem x (y :: xs) | (No contra) | (Right z) = Right go
     where go : Elem x (y :: xs) -> _|_
           go Here impossible
           go (There w) = z w
