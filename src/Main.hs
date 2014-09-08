module Main 
  ( main 
  , selectTermFrequency
  , termFrequency
  , commonTerms
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow       ((&&&), (>>>))
import           Control.Monad
import           Data.Char           (isAlpha, toLower)
import           Data.Function       (on)
import           Data.List           (group, partition, sort, sortBy)
import           System.Directory
import           System.IO

type Freq = (String, Int)

-- | Given a document tokenize >> filter >> return frequency
tokenize :: FilePath -> IO [Freq]
tokenize document = do
  content <- words <$> readFile document
  let freq = termFrequency $ filterCorpus content
  return freq

-- | Clean up the corpus and partition by identity
filterCorpus :: [String] -> [[String]]
filterCorpus =
    let clean = (map toLower . filter isAlpha)
    in map clean >>> sort >>> group

termFrequency :: [[String]] -> [Freq]
termFrequency = map (\(x:_) -> (x, (length x)))

sortedFrequency :: [Freq] -> [Freq]
sortedFrequency = sortBy (compare `on` snd)

sorted :: FilePath -> IO [Freq]
sorted file = sortedFrequency <$> tokenize file

-- | Fetch the term frequency
selectTermFrequency :: Eq a => a -> [(a, t)] -> [t]
selectTermFrequency t xs =
  [y | (x,y) <- xs, x == t]

wordCount :: [String] -> [(String, Int)]
wordCount = map $ (head &&& length) . group

commonTerms :: FilePath -> Int -> IO [String]
commonTerms document n = do
  f <- sorted document
  let r = map fst $ take n $ reverse f
  return r

{- Main -}

main = do
  f <- sorted "corpus/document.txt"
  let r = selectTermFrequency "document" f
  return r
