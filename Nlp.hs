module Nlp
where

import Control.Arrow
import Control.Monad

import Data.Char(isAlpha, toLower)

import System.Directory
import System.IO

import Data.List(group, partition, sort)

type Freq = (String, Int)

-- | Clean up the corpus and partition by identity
filterCorpus :: [String] -> [[String]]
filterCorpus = let clean = (map toLower . filter isAlpha) in
  map clean >>> sort >>> group

termFrequency :: [[String]] -> [(String, Int)]
termFrequency corpus = map (\(x:_) -> (x, (length x))) corpus

-- | Given a document tokenize >> filter >> return frequency
tokenize document = do
  content <- liftM words $ readFile document
  let freq = termFrequency $ filterCorpus content
  return freq
