module Parser

import Parser.Internal

public many : Parser a -> Parser (List a)
many p = go <|> pure []
 where go = do
         v <- p
         vs <- many p
         pure (v :: vs)

{-
takeWhile : (Char -> Bool) -> Parser String
takeWhile f = map pack go
 where go = do
         c <- anyChar
-}
