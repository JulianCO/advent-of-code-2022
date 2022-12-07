#!/usr/bin/env cabal

{- cabal:
build-depends:  base
                , attoparsec
                , text
-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, many', parseOnly)
import Data.Either (fromRight)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

newtype Puzzle = Puzzle [(ElfRange, ElfRange)]

data ElfRange = ElfRange !Int !Int

parseRange :: Parser ElfRange
parseRange = ElfRange <$> (decimal <* char '-') <*> decimal

parseLine :: Parser (ElfRange, ElfRange)
parseLine = ((,) <$> (parseRange <* char ',') <*> parseRange) <* endOfLine

parsePuzzle :: Parser Puzzle
parsePuzzle = Puzzle <$> many' parseLine

rangesContain :: (ElfRange, ElfRange) -> Bool
rangesContain (ElfRange x y, ElfRange x' y') =
  (y' - y) * (x - x') >= 0

solvePuzzle :: Puzzle -> Int
solvePuzzle (Puzzle rs) = length . filter rangesContain $ rs

main :: IO ()
main = do
  filename <- head <$> getArgs
  puzzleText <- TIO.readFile filename
  let puzzle = fromRight (Puzzle []) $ parseOnly parsePuzzle puzzleText
  print (solvePuzzle puzzle)
