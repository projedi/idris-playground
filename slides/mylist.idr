module mylist

data MyList a = Nil | (::) a (MyList a)

(++) : MyList a -> MyList a -> MyList a
Nil ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

instance Functor MyList where
   map f Nil = Nil
   map f (x :: xs) = f x :: map f xs

instance Applicative MyList where
   pure x = [x]
   [] <$> _ = []
   (f :: fs) <$> xs = map f xs ++ (fs <$> xs)

instance Monad MyList where
   [] >>= _ = []
   (x :: xs) >>= f = f x ++ (xs >>= f)

test : MyList Int
test = do
   f <- [id, (*2)]
   x <- [3, 4]
   return $ f x
