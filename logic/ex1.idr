module ex1

import Parser

data Expression
  = Var String
  | Const Int
  | Add Expression Expression
  | Mul Expression Expression

simplify1 : Expression -> Expression
simplify1 (Add (Const m) (Const n)) = Const (m + n)
simplify1 (Mul (Const m) (Const n)) = Const (m * n)
simplify1 (Add (Const 0) x) = x
simplify1 (Add x (Const 0)) = x
simplify1 (Mul (Const 0) x) = Const 0
simplify1 (Mul x (Const 0)) = Const 0
simplify1 (Mul (Const 1) x) = x
simplify1 (Mul x (Const 1)) = x
simplify1 x = x

simplify : Expression -> Expression
simplify (Add e1 e2) = simplify1 (Add (simplify e1) (simplify e2))
simplify (Mul e1 e2) = simplify1 (Mul (simplify e1) (simplify e2))
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
       ops = [[ Infix ((oper "*" <|> oper "") $> pure Mul) AssocLeft ], [ Infix (oper "+" $> pure Add) AssocLeft ]]
       mutual
         simpleExpression : Parser Expression
         simpleExpression = [| Var identifier |] <|> [| Const number |] <|> parens expression
         expression : Parser Expression
         expression = buildExpressionParser ops simpleExpression

onExpr : (Expression -> Expression) -> String -> Maybe String
onExpr f str = [| (prettyPrint . f) (parse str) |]
