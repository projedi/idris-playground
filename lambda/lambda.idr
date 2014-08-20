module lambda

import Control.Monad.State

findWithIndex : (f : a -> b) -> (x : b) -> (v : Vect n a) -> Maybe (i : Fin n ** f (index i v) = x)
findWithIndex f x [] = Nothing
findWithIndex f x (y :: ys) with (findWithIndex f x ys)
  findWithIndex f x (y :: ys) | Nothing = Nothing
  findWithIndex f _ (y :: ys) | Just (i ** refl) = Just (fS i ** refl)

execState : State s a -> s -> a
execState m s = fst $ runIdentity $ runStateT m s

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

newTyVar : (MonadState Int m) => m Ty
newTyVar = do
  s <- get
  put (s + 1)
  pure (TyVar s)

data TyAssignment : Type where
  (<<-) : Int -> Ty -> TyAssignment

updateType : TyAssignment -> Ty -> Ty
updateType ((<<-) i t) (TyVar j) = if i == j then t else TyVar j
updateType f (TyArr t1 t2) = TyArr (updateType f t1) (updateType f t2)

updateTypeMany : List TyAssignment -> Ty -> Ty
updateTypeMany = flip (foldl (flip updateType))

handleApplication : (MonadState Int m) => (t1 : Ty) -> (t2 : Ty) -> m (Maybe (t3 : Ty ** (as : List TyAssignment ** (TyArr (updateTypeMany as t2) t3 = updateTypeMany as t1))))
handleApplication = ?handleApplication_1

inferTypeInCtx : (MonadState Int m) => (ctx : Vect n (Ty, String)) -> (term : Term) -> m (InferType ctx term)
inferTypeInCtx ctx (Var x) =
  case findWithIndex snd x ctx of
       Just (i ** p) => pure $ InferTypeSuccess (fst (index i ctx)) (TypedVar i) ?lem_inferTypeInCtx_1
       Nothing => pure InferTypeFailure
inferTypeInCtx ctx (Lam x t) = do
  tyvar <- newTyVar
  inf <- inferTypeInCtx ((tyvar, x) :: ctx) t
  case inf of
       InferTypeSuccess ty term' p => pure $ InferTypeSuccess (TyArr tyvar ty) (TypedLam x tyvar term') ?lem_inferTypeInCtx_2
       InferTypeFailure => pure InferTypeFailure
inferTypeInCtx ctx (App t1 t2) = do
  inf1 <- inferTypeInCtx ctx t1
  inf2 <- inferTypeInCtx ctx t2
  case (inf1, inf2) of
       (InferTypeFailure, _) => pure InferTypeFailure
       (_, InferTypeFailure) => pure InferTypeFailure
       (InferTypeSuccess ty1 term1 p1, InferTypeSuccess ty2 term2 p2) => do
         res <- handleApplication ty1 ty2
         case res of
              Nothing => pure InferTypeFailure
              Just (ty' ** (tyAssigns ** _)) => ?inferTypeInCtx_1

inferType : (term : Term) -> InferType [] term
inferType term = execState (inferTypeInCtx [] term) (the Int 0)
