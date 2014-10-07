module Parser

import Control.Monad.State

%default total
%access public

abstract record Parser : (a : Type) -> Type where
  MkParser : (parserState : StateT (List Char) Maybe a) -> Parser a

parse : Parser a -> String -> Maybe a
parse p str = map fst $ runStateT (parserState p) (unpack str)

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

private getInput : Parser (List Char)
getInput = MkParser { parserState = get }

private setInput : List Char -> Parser ()
setInput inp = MkParser { parserState = put inp }

eof : Parser ()
eof = do
  inp <- getInput
  case inp of
       [] => pure ()
       _ => empty

anyChar : Parser Char
anyChar = do
  inp <- getInput
  case inp of
       [] => empty
       (x :: xs) => setInput xs $> pure x

char : Char -> Parser Char
char c = do
  x <- anyChar
  when (x /= c) empty
  return c

string : String -> Parser ()
string str = for_ (unpack str) char

takeWhile : (Char -> Bool) -> Parser String
takeWhile f = [| pack go |]
  where -- TODO: total because go is called only when anyChar has consumed some input
        go : Parser (List Char)
        go =  (do x <- anyChar
                  if f x then [| pure x :: assert_total go |] else empty)
          <|> return []

partial many : Parser a -> Parser (List a)
many p = [| p :: many p |] <|> [| [] |]

anyOf : List Char -> Parser Char
anyOf (c :: cs) = (char c $> return c) <|> anyOf cs
anyOf [] = empty

whitespace : Parser ()
whitespace = anyOf [' ', '\t', '\n'] $> return ()

partial whitespaces : Parser ()
whitespaces = many whitespace $> return ()
