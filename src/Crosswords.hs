module Crosswords where

import Data.Map as Map


type Point = (Int, Int)
data Orientation = Horizontal | Vertical

data Word = Word { orientation :: Orientation,
                   text :: String }

newtype Crossword = Crossword (Map Point Word) -- dense representation

empty :: Crossword
empty = Crossword Map.empty

numWords :: Crossword -> Int
numWords (Crossword mapping) = Map.size mapping

class Sizeable a where
  width :: a -> Int
  height :: a -> Int

class Rated a where
  rating :: Fractional b => a -> b

instance Sizeable Crossword where
  width = undefined
  height = undefined

instance Rated Crossword where
  rating crossword = fromIntegral (area * (1 + sidesDiff)) / fromIntegral (numWords crossword) where
    area = w * h
    sidesDiff = abs $ w - h
    w = width crossword
    h = height crossword

instance Show Crossword where
  show crossword = undefined

instance Eq Crossword where
  c1 == c2 = (rating c1) == (rating c2)

instance Ord Crossword where
  compare c1 c2 = compare (rating c1) (rating c2)


generate :: [String] -> [Crossword]
generate words = generateAux words [Crosswords.empty]

generateAux :: [String] -> [Crossword] -> [Crossword]
generateAux [] crosswords = crosswords
generateAux (word : words) crosswords = generateAux words newCrosswords where
  newCrosswords = concatMap (tryInsert word) crosswords


tryInsert :: String -> Crossword -> [Crossword]
tryInsert = undefined


