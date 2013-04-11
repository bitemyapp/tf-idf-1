module Main
where

import Control.Arrow
import Control.Monad

import Data.Function(on)
import Data.Char(isAlpha, toLower)

import System.Directory
import System.IO

import Data.List(group, partition, sort, sortBy)

type Freq = (String, Int)

-- | Clean up the corpus and partition by identity
filterCorpus :: [String] -> [[String]]
filterCorpus = let clean = (map toLower . filter isAlpha) in
                 map clean >>> sort >>> group

termFrequency :: [[String]] -> [Freq]
termFrequency corpus =
  map (\(x:_) -> (x, (length x))) corpus

sortedFrequency :: [Freq] -> [Freq]
sortedFrequency f = sortBy (compare `on` snd) f

sorted :: FilePath -> IO [Freq]
sorted file = sortedFrequency `liftM` tokenize file

-- Fetch the term frequency
selectTermFrequency :: Eq a => a -> [(a, t)] -> [t]
selectTermFrequency t xs =
  [y | (x,y) <- xs, x == t]

-- | Given a document tokenize >> filter >> return frequency
tokenize document = do
  content <- liftM words $ readFile document
  let freq = termFrequency $ filterCorpus content
  return freq

{- Main -}

main = do
  f <- sorted "corpus/document.txt"
  let r = selectTermFrequency "document" f
  return r

