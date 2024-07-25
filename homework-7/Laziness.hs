-- exercise 1: fibonacci numbers
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 .. ]

-- this is freaking awesome
fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)
-- for 3
-- 1 : 1 : [1 + 1 ..]
-- for 4
-- 1 : 1 : [2, 1 + 2 ..]
-- for 5
-- 1 : 1 : [2, 3, 2 + 3 ..]

fastFib :: Integer -> Integer
fastFib n = last (take (fromIntegral n) fibs2)

-- exercise 2: streams
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = go (20 :: Integer)
    where
      go 0 _  = "..."
      go i (Cons x xs) = show x ++ ", " ++ go (i-1) xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f s0 = Cons s0 (streamIterate f (f s0))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x1 xs1) s2 = Cons x1 (streamInterleave s2 xs1)

nats :: Stream Integer
nats = streamIterate (+ 1) 0

-- wow, this is amazing (freaking awesome)
-- https://oeis.org/A007814
ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) (streamMap (+ 1) ruler)

-- exercise 3: supply monad
data Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = S (\(Cons x xs) -> (x, xs))

pureSupply :: a -> Supply s a
pureSupply x = S (\xs -> (x, xs))

mapFirstTuple :: (a -> c) -> (a, b) -> (c, b)
mapFirstTuple f (a, b) = (f a, b)

mapFirstTuple2 :: (a -> b -> c) -> (a, d) -> (b, e) -> (c, d)
mapFirstTuple2 f (a, d) (b, _) = (f a b, d)

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S sf) = S (mapFirstTuple f . sf)

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S sf1) (S sf2) = S go
 where
   go stream = mapFirstTuple2 f (sf1 stream) (sf2 stream)

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S sf) f = S go
  where
    go stream = sf' s
      where
        (a, s) = sf stream
        (S sf') = f a

runSupply :: Stream s -> Supply s a -> a
runSupply stream (S sf) = fst $ sf stream

instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  (>>=) = bindSupply

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Leaf _) = fmap Leaf get
    go (Node l r) = do
      l' <- go l
      r' <- go r
      return (Node l' r')

main :: IO ()
main = do
  putStrLn "Homework 7: Laziness"
  putStrLn "Exercise 1: Fibonacci numbers"
  let fibN = 69 :: Integer
  putStrLn ("Fast fib for " ++ show fibN ++ " is: " ++ show (fastFib fibN))
  let streamI = (19 :: Integer)
  putStrLn ("Stream of " ++ show streamI ++ " is: " ++ show (streamRepeat streamI))
  putStrLn ("streamIterate: " ++ show (streamIterate ('x' :) "o"))
  putStrLn ("streamInterleave: " ++ show (streamInterleave (streamRepeat (0 :: Integer)) (streamRepeat 1)))
  putStrLn ("nats: " ++ show nats)
  putStrLn ("ruler: " ++ show ruler)
  putStrLn "Exercise 3: The Supply monad"
  let l = Leaf ()
  let n = Node
  let t = n (n (n l l) l) (n l l)
  putStrLn ("labelTree: " ++ show (labelTree t))
