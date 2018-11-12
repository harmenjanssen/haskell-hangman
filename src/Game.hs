module Game where

import Puzzle
import WordList (maxWordLength)
import Gallows (buildGallows)

import Control.Monad (forever)
import System.Exit (exitSuccess)


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!\n"
            return puzzle
        (True, _) -> do
            putStrLn "Yeah! 🎉\n"
            return (fillChar puzzle guess)
        (False, _) -> do
            putStrLn "Nope, letter not found. 🚫\n"
            return (fillChar puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle word _ guessed) = case (length $ wrongGuesses puzzle) > maxWordLength of
    True ->
        do putStr $ buildGallows (length $ wrongGuesses puzzle)
           putStrLn "You lose!"
           putStrLn $ "The word was: " ++ word
           exitSuccess
    False -> return ()

gameWin :: Puzzle -> IO ()
gameWin puzzle =
    if wordIsGuessed puzzle
    then
        do putStrLn "You win!"
           exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    putStrLn ""
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"


