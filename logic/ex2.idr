module ex2

import Parser

data Formula a
  = False
  | True
  | Atom a
  | Not (Formula a)
  | And (Formula a) (Formula a)
  | Or (Formula a) (Formula a)
  | Imp (Formula a) (Formula a)
  | Iff (Formula a) (Formula a)
  | Forall String (Formula a)
  | Exists String (Formula a)

onatoms : (a -> Formula b) -> Formula a -> Formula b
onatoms _ False = False
onatoms _ True = True
onatoms g (Atom x) = g x
onatoms g (Not f) = Not (onatoms g f)
onatoms g (And f1 f2) = And (onatoms g f1) (onatoms g f2)
onatoms g (Or f1 f2) = Or (onatoms g f1) (onatoms g f2)
onatoms g (Imp f1 f2) = Imp (onatoms g f1) (onatoms g f2)
onatoms g (Iff f1 f2) = Iff (onatoms g f1) (onatoms g f2)
onatoms g (Forall x f) = Forall x (onatoms g f)
onatoms g (Exists x f) = Exists x (onatoms g f)

instance Functor Formula where
  map _ False = False
  map _ True = True
  map g (Atom x) = Atom (g x)
  map g (Not f) = Not (map g f)
  map g (And f1 f2) = And (map g f1) (map g f2)
  map g (Or f1 f2) = Or (map g f1) (map g f2)
  map g (Imp f1 f2) = Imp (map g f1) (map g f2)
  map g (Iff f1 f2) = Iff (map g f1) (map g f2)
  map g (Forall x f) = Forall x (map g f)
  map g (Exists x f) = Exists x (map g f)

instance Applicative Formula where
  pure x = Atom x
  f <$> x = onatoms (\y => onatoms (pure . y) x) f

instance Monad Formula where
  (>>=) = flip onatoms

identifier : Parser String
identifier = lexeme $ map pack $ takeWhile1 isAlpha

parse : Parser a -> String -> Maybe (Formula a)
parse atomParser = Parser.Internal.parse (between spaces endOfInput expression)
 where parens : Parser b -> Parser b
       parens = between (lexeme $ char '(') (lexeme $ char ')')
       oper : String -> Parser ()
       oper op = lexeme $ string op
       ops : OperatorTable (Formula a)
       ops =
         [ [ Prefix (oper "~" $> pure Not) ]
         , [ Infix (oper "&" $> pure And) AssocLeft ]
         , [ Infix (oper "|" $> pure Or) AssocLeft ]
         , [ Infix (oper "==>" $> pure Imp) AssocRight ]
         , [ Infix (oper "<=>" $> pure Iff) AssocLeft ]
         ]
       constParser : Parser (Formula a)
       constParser = (oper "T" $> pure True) <|> (oper "F" $> pure False)
       mutual
         simpleExpression : Parser (Formula a)
         simpleExpression = constParser <|> [| Atom atomParser |] <|> parens expression
         expression : Parser (Formula a)
         expression = buildExpressionParser ops simpleExpression

parseString : String -> Maybe (Formula String)
parseString = parse identifier

prettyPrint : (a -> String) -> Formula a -> String
prettyPrint atomPrettyPrinter = go (-9000)
 where withParens : Int -> Int -> String -> String
       withParens pprev pnew str = if pprev > pnew then "(" ++ str ++ ")" else str
       go : Int -> Formula a -> String
       go _ False = "F"
       go _ True = "T"
       go _ (Atom a) = atomPrettyPrinter a
       go p (Not f) = withParens p 9000 ("~" ++ go 9000 f)
       go p (And f1 f2) = withParens p 3 (go 3 f1 ++ " & " ++ go 3 f2)
       go p (Or f1 f2) = withParens p 2 (go 2 f1 ++ " | " ++ go 2 f2)
       go p (Imp f1 f2) = withParens p 1 (go 1 f1 ++ " ==> " ++ go 1 f2)
       go p (Iff f1 f2) = withParens p 0 (go 0 f1 ++ " <=> " ++ go 0 f2)

prettyPrintString : Formula String -> String
prettyPrintString = prettyPrint id
