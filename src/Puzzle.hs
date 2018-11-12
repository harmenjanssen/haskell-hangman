module Puzzle where

import Data.Maybe (isJust)
import Data.List (intersperse)
import Gallows (buildGallows)


data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show puzzle@(Puzzle _ discovered guessed) =
        buildGallows (length (wrongGuesses puzzle))
        ++ "\n\n"
        ++ (intersperse ' ' $
         fmap renderPuzzleChar discovered)
        ++ renderGuesses guessed


wrongGuesses :: Puzzle -> [Char]
wrongGuesses (Puzzle word _ guessed) = filter (not . (`elem` word)) guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just a) = a
renderPuzzleChar _ = '_'

renderGuesses :: String -> String
renderGuesses [] = ""
renderGuesses guessed = " Guessed so far: " ++ guessed

fillChar :: Puzzle -> Char -> Puzzle
fillChar (Puzzle word filled guessed) c =
    Puzzle word newFilled (c : guessed)
    where
        zipper guessedChar wordChar prevFilled =
            if wordChar == guessedChar
            then Just wordChar
            else prevFilled
        newFilled = zipWith (zipper c) word filled

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

wordIsGuessed :: Puzzle -> Bool
wordIsGuessed (Puzzle _ filled _) = all isJust filled

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

