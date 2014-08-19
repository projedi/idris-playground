module lambda

data Term : Type where
  Var : String -> Term
  Lam : String -> Term -> Term
  App : Term -> Term -> Term

data Ty : Type where
  TyVar : Int -> Ty
  TyArr : Ty -> Ty -> Ty

data TypedTerm : (ctx : Vect n Ty) -> Ty -> Type where
  TypedVar : (i : Fin n) -> TypedTerm ctx (index i ctx)
  TypedLam : (tvar : Ty) -> TypedTerm (tvar :: ctx) t -> TypedTerm ctx (TyArr tvar t)
  TypedApp : TypedTerm ctx (TyArr t1 t2) -> TypedTerm ctx t1 -> TypedTerm ctx t2
