module Parser

import Parser.Internal

public many : Parser a -> Parser (List a)
many p = go <|> pure []
 where go = do
         v <- p
         vs <- many p
         pure (v :: vs)

public many1 : Parser a -> Parser (List a)
many1 p = [| p :: (many p) |]

public satisfy : (Char -> Bool) -> Parser Char
satisfy f = do
  c <- anyChar
  if f c then pure c else empty

public takeWhile : (Char -> Bool) -> Parser (List Char)
takeWhile f = many (satisfy f)

public takeWhile1 : (Char -> Bool) -> Parser (List Char)
takeWhile1 f = many1 (satisfy f)

public between : Parser before -> Parser after -> Parser a -> Parser a
between before after p = before $> p <$ after

public char : Char -> Parser Char
char c = satisfy (== c)

public parens : Parser a -> Parser a
parens = between (char '(') (char ')')

public integer : Parser Int
integer = map (foldl go 0) (takeWhile1 isDigit)
 where go acc elt = acc * 10 + (prim__charToInt elt - 48)
