module Parser.Expression

import Parser.Combinator

-- Pretty much stolen from Parsec (Actually, attoparsec-expr)

public data Assoc = AssocNone | AssocLeft | AssocRight

public data Operator a
  = Infix (Parser (a -> a -> a)) Assoc
  | Prefix (Parser (a -> a))
  | Postfix (Parser (a -> a))

public OperatorTable : Type -> Type
OperatorTable a = List (List (Operator a))

private OperatorSplitted : Type -> Type
OperatorSplitted a
  = ( Parser (a -> a -> a), Parser (a -> a -> a), Parser (a -> a -> a)
    , Parser (a -> a), Parser (a -> a)
    )

private splitOp :  Operator a -> OperatorSplitted a -> OperatorSplitted a
splitOp (Infix op assoc) (rassoc, lassoc, nassoc, pref, postf) =
  case assoc of
    AssocNone => (rassoc, lassoc, op <|> nassoc, pref, postf)
    AssocLeft => (rassoc, op <|> lassoc, nassoc, pref, postf)
    AssocRight => (op <|> rassoc, lassoc, nassoc, pref, postf)
splitOp (Prefix op) (rassoc, lassoc, nassoc, pref, postf) =
  (rassoc, lassoc, nassoc, op <|> pref, postf)
splitOp (Postfix op) (rassoc, lassoc, nassoc, pref, postf) =
  (rassoc, lassoc, nassoc, pref, op <|> postf)

private makeParser : Parser a -> List (Operator a) -> Parser a
makeParser term ops = do
  let (rassoc, lassoc, nassoc, pref, postf) = foldr splitOp (empty, empty, empty, empty, empty) ops
  x <- termP pref postf
  rassocP pref postf rassoc x <|> lassocP pref postf lassoc x <|> nassocP pref postf nassoc x <|> pure x
 where termP : Parser (a -> a) -> Parser (a -> a) -> Parser a
       termP pref postf = do
         pre <- pref <|> pure id
         x <- term
         post <- postf <|> pure id
         return $ post $ pre x
       rassocP : Parser (a -> a) -> Parser (a -> a) -> Parser (a -> a -> a) -> a -> Parser a
       rassocP pref postf rassoc x = do
         f <- rassoc
         y <- (termP pref postf >>= \x => (rassocP pref postf rassoc x <|> pure x))
         pure $ f x y
       lassocP : Parser (a -> a) -> Parser (a -> a) -> Parser (a -> a -> a) -> a -> Parser a
       lassocP pref postf lassoc x = do
         f <- lassoc
         y <- termP pref postf
         let z = f x y
         lassocP pref postf lassoc z <|> pure z
       nassocP : Parser (a -> a) -> Parser (a -> a) -> Parser (a -> a -> a) -> a -> Parser a
       nassocP pref postf nassoc x = do
         f <- nassoc
         y <- termP pref postf
         pure $ f x y

public buildExpressionParser : OperatorTable a -> Parser a -> Parser a
buildExpressionParser optable simpleExpr = foldl makeParser simpleExpr optable
