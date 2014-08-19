module lambda

data Term : Type where
  Var : String -> Term
  Lam : String -> Term -> Term
  App : Term -> Term -> Term

instance Eq Term where
  (Var x1) == (Var x2) = x1 == x2
  (Lam x1 t1) == (Lam x2 t2) = x1 == x2 && t1 == t2
  (App t1 t2) == (App t3 t4) = t1 == t3 && t2 == t4
  _ == _ = False

data Ty : Type where
  TyVar : Int -> Ty
  TyArr : Ty -> Ty -> Ty

data TypedTerm : (ctx : Vect n (Ty, String)) -> Ty -> Type where
  TypedVar : (i : Fin n) -> TypedTerm ctx (fst $ index i ctx)
  TypedLam : (x : String) -> (tvar : Ty) -> TypedTerm ((tvar, x) :: ctx) t -> TypedTerm ctx (TyArr tvar t)
  TypedApp : TypedTerm ctx (TyArr t1 t2) -> TypedTerm ctx t1 -> TypedTerm ctx t2

dropTypes : (ctx : Vect n (Ty, String)) -> TypedTerm ctx ty -> Term
dropTypes ctx (TypedVar i) = Var (snd $ index i ctx)
dropTypes ctx (TypedLam x tvar t) = Lam x (dropTypes ((tvar, x) :: ctx) t)
dropTypes ctx (TypedApp t1 t2) = App (dropTypes ctx t1) (dropTypes ctx t2)

data InferType : (ctx : Vect n (Ty, String)) -> (term : Term) -> Type where
  InferTypeSuccess : (ty : Ty) -> (term' : TypedTerm ctx ty) -> dropTypes ctx term' = term -> InferType ctx term
  InferTypeFailure : InferType ctx term

inferTypeInCtx : (ctx : Vect n (Ty, String)) -> (term : Term) -> InferType ctx term
inferTypeInCtx ctx term = ?go

inferType : (term : Term) -> InferType [] term
inferType = inferTypeInCtx []
