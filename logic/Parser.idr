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

{-
takeWhile : (Char -> Bool) -> Parser String
takeWhile f = map pack go
 where go = do
         c <- anyChar
-}
