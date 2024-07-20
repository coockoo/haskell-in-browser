import Data.Char (isAscii)
import Data.Semigroup (Semigroup(stimes))

halveEvens :: [Integer] -> [Integer]
halveEvens = foldr (\x acc -> if even x then (x `div` 2) : acc else acc) []

safeString :: String -> String
safeString = foldr (\x acc -> (if isAscii x then x else '_') : acc) ""

holes :: [a] -> [[a]]
holes list = map doDrop (zip [0..] clone)
 where
   clone = stimes (length list) [list]
   doDrop (i, l) = take i l ++ drop (i+1) l

main :: IO()
main =
  putStr "Homework 5: Exercise 1: Lists, lists, lists"
  <> putStr "halveEvens: " <> print (halveEvens [6,6,6,3,3,3,2,2,2])
  <> putStr "safeString: " <> print (safeString "🙋.o")
  <> putStr "holes: " <> print (holes "Hello")
