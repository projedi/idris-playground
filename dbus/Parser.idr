module Parser

import Control.Monad.State

record Parser : (a : Type) -> Type where
  MkParser : (parserState : StateT (List Char) Maybe a) -> Parser a

eof : Parser ()
eof = MkParser
  { parserState = ST go
  }
  where go : List Char -> Maybe ((), List Char)
        go [] = Just ((), [])
        go _ = Nothing

string : String -> Parser ()
string str = MkParser
  { parserState = ST $ go (unpack str)
  }
  where go : List Char -> List Char -> Maybe ((), List Char)
        go [] input = Just ((), input)
        go _ [] = Nothing
        go (x :: str) (y :: input) = if x == y then go str input else Nothing

anyChar : Parser Char
anyChar = MkParser
  { parserState = ST go
  }
  where go : List Char -> Maybe (Char, List Char)
        go [] = Nothing
        go (x :: input) = Just (x, input)
