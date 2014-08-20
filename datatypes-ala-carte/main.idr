module main

infixr 5 :+:

data (:+:) : (f : Type -> Type) -> (g : Type -> Type) -> (e : Type) -> Type where
  Inl : f e -> (f :+: g) e
  Inr : g e -> (f :+: g) e

instance (Functor f, Functor g) => Functor (f :+: g) where
  map f (Inl e) = Inl (map f e)
  map f (Inr e) = Inr (map f e)

data Expr : (f : Type -> Type) -> Type where
  In : f (Expr f) -> Expr f

foldExpr : Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (map (foldExpr f) t)

class Functor f => Eval (f : Type -> Type) where
  evalAlgebra : f Int -> Int

eval : Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr x) = evalAlgebra x

data Val e = MkVal Int

instance Functor Val where
  map f (MkVal x) = MkVal x

instance Eval Val where
  evalAlgebra (MkVal x) = x

data Add e = MkAdd e e

instance Functor Add where
  map f (MkAdd e1 e2) = MkAdd (f e1) (f e2)

instance Eval Add where
  evalAlgebra (MkAdd x y) = x + y

addExample1 : Expr (Val :+: Add)
addExample1 = In (Inr (MkAdd (In (Inl (MkVal 118))) (In (Inl (MkVal 1219)))))

infix 5 :<:

class (:<:) (sub : Type -> Type) (sup : Type -> Type) where
  inj : sub a -> sup a

instance (:<:) f f where
  inj = id

instance [left_type_lt] (f :<: g) => (:<:) f (g :+: h) where
  inj = Inl . inj

instance [right_type_lt] (f :<: h) => (:<:) f (g :+: h) where
  inj = Inr . inj

inject : (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

(+) : (Add :<: f) => Expr f -> Expr f -> Expr f
x + y = inject (MkAdd x y)

val : (Val :<: f) => Int -> Expr f
val x = inject (MkVal x)

addExample2 : Expr (Val :+: Add)
addExample2 = (+) @{right_type_lt} (val @{left_type_lt} 118) (val @{left_type_lt} 1219)

data Mul e = MkMul e e

instance Functor Mul where
  map f (MkMul x y) = MkMul (f x) (f y)

instance Eval Mul where
  evalAlgebra (MkMul x y) = x * y

(*) : (Mul :<: f) => Expr f -> Expr f -> Expr f
x * y = inject (MkMul x y)

mulExample : Expr (Val :+: Add :+: Mul)
mulExample = (+) @{right_type_lt @{left_type_lt}} ((*) @{right_type_lt @{right_type_lt}} (val @{left_type_lt} 80) (val @{left_type_lt} 5)) (val @{left_type_lt} 4)
