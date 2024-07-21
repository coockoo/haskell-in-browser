import Data.Char (isAscii, isNumber, digitToInt)
import Data.Semigroup (Semigroup(stimes))
import Data.List (maximumBy)
import Data.Function (on)

-- challenge for this homework is not to use recursive functions directly,
-- only list stuff

halveEvens :: [Integer] -> [Integer]
halveEvens = foldr (\x acc -> if even x then (x `div` 2) : acc else acc) []

safeString :: String -> String
safeString = map (\x -> (if isAscii x then x else '_'))

holes :: [a] -> [[a]]
holes list = zipWith doDrop [0..] clone
 where
   clone = stimes (length list) [list]
   doDrop i l = take i l ++ drop (i+1) l

longestText :: Show a => [a] -> a
longestText = maximumBy (compare `on` length . show)

adjacents :: [a] -> [(a,a)]
adjacents list = zipWith go [0..] clone
  where
    clone = stimes (length list - 1) [list]
    firstAndLast l = (head l, last l)
    go i l = firstAndLast $ take 2 $ drop i l

commas :: [String] -> String
commas list = unwords $ zipWith go [0 ..] list
  where
    go i l = if i == length list - 1 then l else l ++ ","

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials list = foldr (zipWith (+)) (head list) (drop 1 list)

sumNumbers :: String -> Integer
sumNumbers str = res
  where
    (res, _) = foldr go (0, 1) str
    go :: Char -> (Integer, Integer) -> (Integer, Integer)
    go el (acc, times) = if isNumber el
                         then (acc + times * toInteger (digitToInt el), times * 10)
                         else (acc, 1)

main :: IO()
main =
  putStr "Homework 5: Exercise 1: Lists, lists, lists\n"
  <> putStr "halveEvens: " <> print (halveEvens [6,6,6,3,3,3,2,2,2])
  <> putStr "safeString: " <> print (safeString "🙋.o")
  <> putStr "holes: " <> print (holes "Hello")
  <> putStr "longestText: " <> print (longestText [2::Int,4,16,32])
  <> putStr "adjacents: " <> print (adjacents "Hello")
  <> putStr "commas: " <> print (commas ["Hello", "World", "Kappa"])
  <> putStr "addPolynomials: " <> print (addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]])
  <> putStr "sumNumbers: " <> print (sumNumbers "words0are1234separated12by3integers45678")

