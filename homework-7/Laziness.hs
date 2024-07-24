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

main :: IO ()
main = print (fastFib 500)
