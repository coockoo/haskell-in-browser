halveEvens :: [Integer] -> [Integer]
halveEvens list = go list 1
  where
    go :: Integral a => [a] -> Integer -> [a]
    go [] _ = []
    go (x: xs) idx
      | even idx = (x `div` 2) : go xs (idx + 1)
      | otherwise = go xs (idx + 1)

main :: IO()
main = print $ halveEvens [1,2,3,4,5]
