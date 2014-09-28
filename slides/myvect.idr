module myidr

data MyVect : Nat -> (a : Type) -> Type where
   Nil : MyVect 0 a
   (::) : a -> MyVect n a -> MyVect (S n) a

(++) : MyVect n a -> MyVect m a -> MyVect (n + m) a
[] ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

infix 9 !!

(!!) : MyVect n a -> Fin n -> a
(x :: xs) !! fZ = x
(x :: xs) !! (fS y) = xs !! y
