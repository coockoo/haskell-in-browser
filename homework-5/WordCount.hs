import Data.List (nub, maximumBy)
import Data.Function (on)
import Data.Semigroup (Semigroup(stimes))

adjacents :: [a] -> [(a,a)]
adjacents list = zipWith go [0..] clone
  where
    clone = stimes (length list - 1) [list]
    firstAndLast l = (head l, last l)
    go i l = firstAndLast $ take 2 $ drop i l

wordCount :: String -> String
wordCount str =
  "Number of lines: " ++ numberOfLines ++ "\n" ++
  "Number of empty lines: " ++ numberOfEmptyLines ++ "\n" ++
  "Number of words: " ++ numberOfWords ++ "\n" ++
  "Number of unique words: " ++ numberOfUniqueWords ++ "\n" ++
  "Number of words followed by themselves: " ++ numberOfFollowers ++ "\n" ++
  "Length of the longest line: " ++ lline ++ "\n"
  where
    numberOfLines = show (length (lines str))
    numberOfEmptyLines = show (length (filter null (lines str)))
    numberOfWords = show (length (words str))
    numberOfUniqueWords = show (length (nub (words str)))
    numberOfFollowers = show $
      foldr
      (\(a, b) acc -> if a == b then acc + 1 else acc)
      (0 :: Integer)
      (adjacents (words str))
    lline = show (length (maximumBy (compare `on` length) (lines str)))

main :: IO()
main = putStr (wordCount "123\n4567\n123\n123\n")
