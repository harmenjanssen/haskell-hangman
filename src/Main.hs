module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


newtype WordList = WordList [String] deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $
         fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where gameLength w =
            let l = length w
            in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, (length wl - 1))
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just a) = a
renderPuzzleChar _ = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filled guessed) c =
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

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word, filling in the word accordingly."
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character wasn't in the word, try again."
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) =
    if (length guessed) > 7
    then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ word
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filled _) =
    if all isJust filled
    then
        do putStrLn "You win!"
           exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
