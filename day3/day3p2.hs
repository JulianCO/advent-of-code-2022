#!/usr/bin/env cabal

{- cabal:
build-depends:  base
                , split
-}

module Main where

import Data.Char (isLower, ord)
import Data.List.Split (chunksOf)
import System.Environment (getArgs)

findCommonElement :: (Eq a) => [[a]] -> a
findCommonElement [xs, ys, zs] = head [x | x <- xs, x `elem` ys, x `elem` zs]

letterToPriority :: Char -> Int
letterToPriority c =
  if isLower c
    then ord c - ord 'a' + 1
    else ord c - ord 'A' + 27

processChunk :: [[Char]] -> Int
processChunk = letterToPriority . findCommonElement

main :: IO ()
main = do
  filename <- head <$> getArgs
  puzzleInput <- readFile filename
  let puzzleAnswer = sum . map processChunk . chunksOf 3 $ lines puzzleInput
  print puzzleAnswer