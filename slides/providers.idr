module Main

import Providers

%language TypeProviders

strToType : String -> Type
strToType "Int" = Int
strToType _ = String

fromFile : String -> IO (Provider Type)
fromFile fname = return $ Provide $ strToType $ trim !(readFile fname)

%provide (T : Type) with fromFile "config.h"

f : T
f = 42

main : IO ()
main = print f
