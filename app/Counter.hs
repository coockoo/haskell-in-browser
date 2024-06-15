foreign export ccall fib :: Int -> Int

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- for some reason, without main it does not work
main :: IO ()
main = return ()
