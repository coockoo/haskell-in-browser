import Data.Char (isAscii)
import Data.Semigroup (Semigroup(stimes))
import Data.List (maximumBy)
import Data.Function (on)

halveEvens :: [Integer] -> [Integer]
halveEvens = foldr (\x acc -> if even x then (x `div` 2) : acc else acc) []

safeString :: String -> String
safeString = map (\x -> (if isAscii x then x else '_'))

holes :: [a] -> [[a]]
holes list = zipWith (curry doDrop) [0..] clone
 where
   clone = stimes (length list) [list]
   doDrop (i, l) = take i l ++ drop (i+1) l

longestText :: Show a => [a] -> a
longestText = maximumBy (compare `on` length . show)

adjacents :: [a] -> [(a,a)]
adjacents list = zipWith (curry go) [0 .. ] clone
  where
    clone = stimes (length list - 1) [list]
    firstAndLast l = (head l, last l)
    go = firstAndLast . take 2 . uncurry drop

main :: IO()
main =
  putStr "Homework 5: Exercise 1: Lists, lists, lists\n"
  <> putStr "halveEvens: " <> print (halveEvens [6,6,6,3,3,3,2,2,2])
  <> putStr "safeString: " <> print (safeString "🙋.o")
  <> putStr "holes: " <> print (holes "Hello")
  <> putStr "longestText: " <> print (longestText [2::Int,4,16,32])
  <> putStr "adjacents: " <> print (adjacents "Hello")
