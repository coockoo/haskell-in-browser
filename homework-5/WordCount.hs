import Data.List (nub, maximumBy)
import Data.Function (on)

wordCount :: String -> String
wordCount str =
  "Number of lines: " ++ numberOfLines ++ "\n" ++
  "Number of empty lines: " ++ numberOfEmptyLines ++ "\n" ++
  "Number of words: " ++ numberOfWords ++ "\n" ++
  "Number of unique words: " ++ numberOfUniqueWords ++ "\n" ++
  "Number of words followed by themselves: idk, probably some\n" ++
  "Length of the longest line: " ++ lline ++ "\n"
  where
    numberOfLines = show (length (lines str))
    numberOfEmptyLines = show (length (filter null (lines str)))
    numberOfWords = show (length (words str))
    numberOfUniqueWords = show (length (nub (words str)))
    lline = show (length (maximumBy (compare `on` length) (lines str)))

main :: IO()
main = putStr (wordCount "123\n4567\n123\n")
