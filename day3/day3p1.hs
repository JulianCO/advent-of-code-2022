#!/usr/bin/env cabal

{- cabal:
build-depends:  base
-}

module Main where

import Data.Char (isLower, ord)
import System.Environment (getArgs)

splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = splitAt (length xs `div` 2) xs

findCommonElement :: (Eq a) => ([a], [a]) -> a
findCommonElement (xs, ys) = head [x | x <- xs, x `elem` ys]

letterToPriority :: Char -> Int
letterToPriority c =
  if isLower c
    then ord c - ord 'a' + 1
    else ord c - ord 'A' + 27

processLine :: [Char] -> Int
processLine = letterToPriority . findCommonElement . splitInHalf

main :: IO ()
main = do
  filename <- head <$> getArgs
  puzzleInput <- readFile filename
  let puzzleAnswer = sum $ map processLine (lines puzzleInput)
  print puzzleAnswer
