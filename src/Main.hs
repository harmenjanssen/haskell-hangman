module Main where

import Data.Char (toLower)

import WordList (randomWord)
import Puzzle (freshPuzzle)
import Game (runGame)

main :: IO ()
main = do
    word <- randomWord
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
