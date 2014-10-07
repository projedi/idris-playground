module Parser

import Control.Monad.State

record Parser : (a : Type) -> Type where
  MkParser : (parserState : StateT (List Char) Maybe a) -> Parser a

instance Functor Parser where
  map f (MkParser st) = MkParser (map f st)

instance Applicative Parser where
  pure x = MkParser (pure x)
  (MkParser f) <$> (MkParser x) = MkParser (f <$> x)

instance Monad Parser where
  (MkParser st) >>= f = MkParser (st >>= (parserState . f))

instance Alternative Parser where
  empty = MkParser $ ST $ \_ => Nothing
  (MkParser st1) <|> (MkParser st2) = MkParser $ ST $ \s =>
    case (runStateT st1 s) of
         Nothing => runStateT st2 s
         Just x => Just x

eof : Parser ()
eof = MkParser
  { parserState = ST go
  }
  where go : List Char -> Maybe ((), List Char)
        go [] = Just ((), [])
        go _ = Nothing

anyChar : Parser Char
anyChar = MkParser
  { parserState = ST go
  }
  where go : List Char -> Maybe (Char, List Char)
        go [] = Nothing
        go (x :: input) = Just (x, input)

char : Char -> Parser ()
char c = do
  x <- anyChar
  when (x /= c) empty

string : String -> Parser ()
string str = for_ (unpack str) char

takeWhile : (Char -> Bool) -> Parser String
takeWhile f = [| pack go |]
  where go : Parser (List Char)
        go = do
          c <- [| Just anyChar |] <|> [| Nothing |]
          maybe (return []) (\x => return $ x :: !go) c
