module Parser

import Control.Monad.State

record Parser : (a : Type) -> Type where
  MkParser : (parserState : StateT (List Char) Maybe a) -> Parser a


