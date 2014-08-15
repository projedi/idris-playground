module Parser

import Control.Monad.State

abstract record Parser : (a : Type) -> Type where
  MkParser : (unParser : StateT String Maybe a) -> Parser a

instance Functor Parser where
  map f (MkParser m) = MkParser (map f m)

instance Applicative Parser where
  pure a = MkParser (pure a)
  (MkParser f) <$> (MkParser x) = MkParser (f <$> x)

instance Alternative Parser where
  empty = MkParser $ ST $ \_ => Nothing
  (MkParser m1) <|> (MkParser m2) = MkParser $ ST $ \s =>
    let v = runStateT m1 s
    in case v of
            Nothing => runStateT m2 s
            Just _ => v

instance Monad Parser where
  (MkParser m) >>= f = MkParser (m >>= (unParser . f))


