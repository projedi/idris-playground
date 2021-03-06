module Parser.Combinator

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

public integer : Parser Int
integer = map (foldl go 0) (takeWhile1 isDigit)
 where go acc elt = acc * 10 + (prim__charToInt elt - 48)

public string : String -> Parser ()
string str = go (unpack str)
 where go [] = pure ()
       go (x :: xs) = char x $> go xs

public space : Parser Char
space = satisfy isSpace

public spaces : Parser ()
spaces = do
  _ <- takeWhile isSpace
  pure ()

public lexeme : Parser a -> Parser a
lexeme p = p <$ spaces
