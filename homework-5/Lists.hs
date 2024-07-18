import Data.Char (isAscii)

halveEvens :: [Integer] -> [Integer]
halveEvens [] = []
halveEvens (x : xs)
  | even x = (x `div` 2) : halveEvens xs
  | otherwise = halveEvens xs

safeString :: String -> String
safeString [] = []
safeString (x : xs)
  | isAscii x = x : safeString xs
  | otherwise = '_' : safeString xs

main :: IO()
main = print $ safeString "🙋.o"
-- main = print $ halveEvens [6,6,6,3,3,3,2,2,2]
