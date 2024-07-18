halveEvens :: [Integer] -> [Integer]
halveEvens [] = []
halveEvens (x : xs)
  | even x = (x `div` 2) : halveEvens xs
  | otherwise = halveEvens xs

main :: IO()
main = print $ halveEvens [6,6,6,3,3,3,2,2,2]
