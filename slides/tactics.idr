module tactics

lemma_applicative_identity : (vs : List a) -> (pure id <$> vs = vs)
lemma_applicative_identity [] = refl
lemma_applicative_identity (v :: vs) =
   let rec = lemma_applicative_identity vs
   in ?lemma_applicative_identity_rhs

---------- Proofs ----------

tactics.lemma_applicative_identity_rhs = proof
  intros
  rewrite rec
  trivial


