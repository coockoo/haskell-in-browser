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
