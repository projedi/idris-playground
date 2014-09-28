module Main

import System

broken : Int -> Int
broken 0 = 1
broken n = n * broken (n - 1)

ifThenElse : Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ f = f

main : IO ()
main = do
  args <- getArgs
  case args of
       [_, v] => print $ ifThenElse (not $ v == "0") 0 (broken (-1))
       _ => putStrLn $ "Expected one command line argument; got " ++ show args
