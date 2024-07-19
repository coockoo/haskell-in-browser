import Data.Char (isAscii)

halveEvens :: [Integer] -> [Integer]
halveEvens = foldr (\x acc -> if even x then (x `div` 2) : acc else acc) []

safeString :: String -> String
safeString = foldr (\x acc -> (if isAscii x then x else '_') : acc) ""

main :: IO()
main = print $ safeString "🙋.o"
-- main = print $ halveEvens [6,6,6,3,3,3,2,2,2]
