module Parser

import Parser.Internal

public many : Parser a -> Parser (List a)
many p = go <|> pure []
 where go = do
         v <- p
         vs <- many p
         pure (v :: vs)

public satisfy : (Char -> Bool) -> Parser Char
satisfy f = do
  c <- anyChar
  if f c then pure c else empty

public takeWhile : (Char -> Bool) -> Parser (List Char)
takeWhile f = many (satisfy f)

public between : Parser before -> Parser after -> Parser a -> Parser a
between before after p = before $> p <$ after
