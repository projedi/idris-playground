module ex1

import Parser

data Expression
  = Var String
  | Const Int
  | Add Expression Expression
  | Mul Expression Expression
  | Exp Expression Expression
  | Sub Expression Expression
  | Neg Expression

instance Eq Expression where
  (Var x) == (Var y) = x == y
  (Const n) == (Const m) = n == m
  (Add e1 e2) == (Add e3 e4) = e1 == e3 && e2 == e4
  (Mul e1 e2) == (Mul e3 e4) = e1 == e3 && e2 == e4
  (Exp e1 e2) == (Exp e3 e4) = e1 == e3 && e2 == e4
  (Sub e1 e2) == (Sub e3 e4) = e1 == e3 && e2 == e4
  (Neg e1) == (Neg e2) = e1 == e2
  _ == _ = False

infixr 11 ^
(^) : Int -> Int -> Int
x ^ y = if y < 0 then 0 else if y == 0 then 1 else x * x ^ (y - 1)

simplify1 : Expression -> Expression
simplify1 (Add (Const m) (Const n)) = Const (m + n)
simplify1 (Mul (Const m) (Const n)) = Const (m * n)
simplify1 (Add (Const 0) x) = x
simplify1 (Add x (Const 0)) = x
simplify1 (Add x (Neg y)) = Sub x y
simplify1 (Mul (Const 0) x) = Const 0
simplify1 (Mul x (Const 0)) = Const 0
simplify1 (Mul (Const 1) x) = x
simplify1 (Mul x (Const 1)) = x
simplify1 (Exp (Const n) (Const m)) = Const (n ^ m)
simplify1 (Exp x (Const 0)) = Const 1
simplify1 (Exp x (Const 1)) = x
simplify1 (Exp (Const 0) x) = Const 0 -- 0^0 will be 1
simplify1 (Exp (Const 1) x) = x
simplify1 (Sub (Const n) (Const m)) = Const (n - m)
simplify1 (Sub x (Const 0)) = x
simplify1 (Sub x (Neg y)) = Add x y
simplify1 (Sub x y) = if x == y then Const 0 else Sub x y
simplify1 (Neg (Const n)) = Const (0 - n)
simplify1 (Neg (Neg x)) = x
simplify1 x = x

simplify : Expression -> Expression
simplify (Add e1 e2) = simplify1 (Add (simplify e1) (simplify e2))
simplify (Mul e1 e2) = simplify1 (Mul (simplify e1) (simplify e2))
simplify (Exp e1 e2) = simplify1 (Exp (simplify e1) (simplify e2))
simplify (Sub e1 e2) = simplify1 (Sub (simplify e1) (simplify e2))
simplify (Neg e) = simplify1 (Neg (simplify e))
simplify x = simplify1 x

prettyPrint : Expression -> String
prettyPrint = go (-9000)
 where withParens : Int -> Int -> String -> String
       withParens pprev pnew str = if pprev > pnew then "(" ++ str ++ ")" else str
       go : Int -> Expression -> String
       go _ (Var v) = v
       go _ (Const i) = show i
       go p (Add e1 e2) = withParens p 0 (go 0 e1 ++ " + " ++ go 0 e2)
       go p (Mul e1 e2) = withParens p 1 (go 1 e1 ++ " * " ++ go 1 e2)
       go p (Exp e1 e2) = withParens p 2 (go 2 e1 ++ " ^ " ++ go 2 e2)
       go p (Sub e1 e2) = withParens p 0 (go 0 e1 ++ " - " ++ go 2 e2)
       go p (Neg e) = withParens p 9000 ("-" ++ go 9000 e)

parse : String -> Maybe Expression
parse = Parser.Internal.parse (between spaces endOfInput expression)
 where parens : Parser a -> Parser a
       parens = between (lexeme $ char '(') (lexeme $ char ')')
       identifier : Parser String
       identifier = lexeme $ map pack $ takeWhile1 isAlpha
       number : Parser Int
       number = lexeme $ integer
       oper : String -> Parser ()
       oper op = lexeme $ string op
       ops : OperatorTable Expression
       ops =
         [ [ Prefix (oper "-" $> pure Neg) ]
         , [ Infix (oper "^" $> pure Exp) AssocRight ]
         , [ Infix (oper "*" $> pure Mul) AssocLeft ]
         , [ Infix (oper "+" $> pure Add) AssocLeft, Infix (oper "-" $> pure Sub) AssocLeft ]
         ]
       mutual
         simpleExpression : Parser Expression
         simpleExpression = [| Var identifier |] <|> [| Const number |] <|> parens expression
         expression : Parser Expression
         expression = buildExpressionParser ops simpleExpression

onExpr : (Expression -> Expression) -> String -> Maybe String
onExpr f str = [| (prettyPrint . f) (parse str) |]
