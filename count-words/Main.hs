-- based on this article whitepaper
-- https://arxiv.org/pdf/2301.10191
-- usage: wc hamlet.txt 1000
module Main where

import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.Random.Stateful as R
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad as M
import Data.Char (toLower, isAlpha)
import System.Environment (getArgs)

andM :: (Monad m, Foldable t) => m (t Bool) -> m Bool
andM xs = and <$> xs

replAndM :: Monad m => Integer -> m Bool -> m Bool
replAndM n m  = andM $ M.replicateM (fromIntegral n) m

stays :: MonadIO m => Integer -> m Bool
stays n = replAndM n $ R.getStdRandom $ R.randomR (True, False)

countWords :: MonadIO m => [String] -> Int -> m Integer
countWords w maxDictSize = do
  (set, _, r) <- goAll Set.empty w 0
  let s = toInteger (Set.size set)
  return (s * (2 ^ r))

  where
    goAll :: MonadIO m => Set String -> [String] -> Integer -> m (Set String, [String], Integer)
    goAll s [] r = return (s, [], r - 1)
    goAll s xs r  = do
      (s', xs') <- go s xs r
      droppedSet <- if Set.size s' == maxDictSize then goDrop s' else return s'
      goAll droppedSet xs' (r + 1)

    goDrop :: MonadIO m => Set String -> m (Set String)
    goDrop t = do
      res <- M.filterM (\_ -> stays 1) (Set.toList t)
      return (Set.fromList res)

    go :: MonadIO m => Set String -> [String] -> Integer -> m (Set String, [String])
    go s [] _ = return (s, [])
    go s xs r =
      let l = Set.size s in
      if l == maxDictSize then
        return (s, xs)
      else do
        let (x:t) = xs
        let s' = if Set.member x s then Set.delete x s else s
        f <- stays r
        if f then go (Set.insert x s') t r else go s' t r

main :: IO ()
main = do
  -- todo: add some comments on how to use
  (filename:maxDictSize:_) <- getArgs
  file <- openFile filename ReadMode
  contents <- hGetContents file
  let w = map (filter isAlpha . map toLower) (words contents)
      s' = Set.size (Set.fromList w)

  s <- countWords w (read maxDictSize)
  print ("Precise: " ++ show s')
  print ("Approx: " ++ show s)
