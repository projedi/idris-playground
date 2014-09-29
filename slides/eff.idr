module Main

import Effect.StdIO
import Effect.State

data Tree a = Leaf a | Node a (Tree a) (Tree a)

tree : Tree Int
tree = Node 3 (Leaf 0) (Node 4 (Node 5 (Leaf 1) (Leaf 2)) (Leaf 6))

dfs : (a -> { [STATE b] } Eff ()) -> Tree a -> { [STDIO, STATE b] } Eff ()
dfs f (Leaf x) = do
  putStrLn "Encountered leaf"
  f x
dfs f (Node x y z) = do
  dfs f y
  f x
  dfs f z

main : IO ()
main = print !(run eff)
 where eff : { [STDIO, STATE Int] } Eff Int
       eff = do
         dfs (\x => update (+x)) tree
         get
