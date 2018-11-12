module WordList where

import System.Random (randomRIO)


newtype WordList = WordList [String] deriving (Eq, Show)

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

randomWord' :: WordList -> IO String
randomWord' (WordList wl) = do
    randomIndex <- randomRIO (0, (length wl - 1))
    return $ wl !! randomIndex

randomWord :: IO String
randomWord = gameWords >>= randomWord'



