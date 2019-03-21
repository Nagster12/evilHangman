-- Keegan Laporte and Noah Nagy
-- CS231 02L Lab8
-- Main.hs
module Main where
import System.Exit
import System.Environment
import System.Directory
import System.IO
import Data.Char
import PlayEvilHangman
import Control.Monad (when)
-- EvilHangman:
-- Program Description
-- Usage: "./Hangman dictFileName WordLength GuessCount [Optional] -n"
-- This file: Parses user inputs and initializes game
-- Support Files: playEvilHangman.hs will perform the game

main = do
  -- Process: Check arguments, and if all input is good, play evilHangman in the proper mode
  -- 1. Open Dictionary
  args <- getArgs
  --Check if the number of arguments
  if ((length args) > 4 || (length args) < 3)
  then do
    putStrLn "Usage: ./Hangman -dictionary name- -length of word- -number of guesses-\n"
    exitSuccess
  else
    return ()

  fileExists <- doesFileExist $ getFileName args
  if (not fileExists)
  
  then -- Bad Dictionary File -- Tell User
    putStr "Dictionary file does not exist\n"
    
  else do -- Good Dictionary File
    dictHandle  <- openFile (getFileName args) ReadMode
    dictContent <- hGetContents dictHandle
    let dictionary = words dictContent
    
    -- 2. Check argument Validity
    let validArgsMsg   = isValidArgs args dictionary
    if (validArgsMsg)  == ""
    then do -- Good Arguments from user --Play Evil Hangman!!
      let wordLen      = getWordLen args
      --let shortenDic   = shortenDictionary dictionary wordLen
      let debugMode = checkDebugMode args
      when ((getGuessCount args) > 15) (putStr "Number of guesses set to maximum of 15\n")
      let guessCount = if ((getGuessCount args) > 15) then 15 else (getGuessCount args)
      --Start game here
      startGame dictionary wordLen guessCount debugMode
    else do -- Bad Arguments -- Tell User they typed something wrong
      putStr $ validArgsMsg ++ "Usage: ./Hangman -dictionary name- -length of word- -number of guesses-\n"

{-

	Function that runs the game, and checks if the user wants to
		play multiple games, and with differing values for
		word length and number of guesses
	Input: original dictionary file, length of word, number of 
		guesses, check if we need to print dictionary length
	

-}
startGame dictionary wordLength guessNumber debugMode = do
    let shortenDic   = shortenDictionary dictionary wordLength
    playEvilHangman shortenDic wordLength guessNumber debugMode
    putStrLn "Play again? yes/no"
    restartGame <- getLine  
    if (restartGame) == "yes" 
    then do
      putStrLn "New word length? yes/no"
      checkNewLength <- hGetLine stdin
      putStrLn "New number of guesses? yes/no"
      checkNewGuess <- hGetLine stdin
      if ((checkNewLength == "yes") && (checkNewGuess == "yes"))
        then do
          putStrLn "New length?" 
          newLength <- getLine
          putStrLn "New guess?"
          newGuess <- getLine
          let newArgs = ["", newLength, newGuess]
          let validArgsMsg   = isValidArgs newArgs dictionary
          if (validArgsMsg)  == ""
           then do 
            when ((getGuessCount newArgs) > 15) (putStr "Number of guesses set to maximum of 15\n")
            let newGuessCount = if ((getGuessCount newArgs) > 15) then 15 else (getGuessCount newArgs)
            startGame dictionary (read newLength) newGuessCount debugMode
          else do 
            putStr $ validArgsMsg ++ "Usage: ./Hangman -dictionary name- -length of word- -number of guesses-\n"
        else do
          if ((checkNewLength == "no") && (checkNewGuess == "yes"))
            then do
              putStrLn "New guess?"
              newGuess <- getLine
              let newArgs = ["", show wordLength, newGuess]
              let validArgsMsg   = isValidArgs newArgs dictionary
              if (validArgsMsg)  == ""
               then do 
                 when ((getGuessCount newArgs) > 15) (putStr "Number of guesses set to maximum of 15\n")
                 let newGuessCount = if ((getGuessCount newArgs) > 15) then 15 else (getGuessCount newArgs)
                 startGame dictionary wordLength newGuessCount debugMode
              else do 
                 putStr $ validArgsMsg ++ "Usage: ./Hangman -dictionary name- -length of word- -number of guesses-\n"
           else do
            if ((checkNewLength == "yes") && (checkNewGuess == "no"))
                  then do
                    putStrLn "New length?" 
                    newLength <- getLine
                    let newArgs = ["", newLength, show guessNumber]
                    let validArgsMsg   = isValidArgs newArgs dictionary
                    if (validArgsMsg)  == ""
                     then do startGame dictionary (read newLength) guessNumber debugMode
                    else do
                      putStr $ validArgsMsg ++ "Usage: ./Hangman -dictionary name- -length of word- -number of guesses-\n"
             else
               startGame dictionary wordLength guessNumber debugMode
     else
       putStrLn "Thanks for playing!"

-- Helper Functions: 


--Create a list of words from dictionary with specified length
shortenDictionary :: [String] -> Int -> [String]
shortenDictionary [] wordLength = []
shortenDictionary (x:xs) wordLength
   | (length x) == wordLength =  x : shortenDictionary xs wordLength
   | otherwise                =  shortenDictionary xs wordLength

-- Checks the main program arguments and returns an error message if one of the inputs is not valid
isValidArgs :: [String] -> [String]-> String
isValidArgs args dictionary
  | badWordLen (getWordLen args) dictionary = "Error: A word of that length does not exist in the selected dictionary\n"
  | (getGuessCount args) < 1                = "Error: You must select a number of guesses between 1 and 15\n"
  | otherwise                               = ""

-- checkDebugMode: Returns True if optional "-n" is found
checkDebugMode :: [String] -> Bool
checkDebugMode args
  | (length args) == 4 && (last args) == "-n" = True
  | otherwise                                 = False

-- badWordLen: Returns True if length is invalid (below 0)
badWordLen :: Int -> [String] -> Bool
badWordLen wordLen dictionary
  | length (filter (matchesLen wordLen) dictionary) > 0  = False -- Good Word Len
  | otherwise                                            = True  -- Bad Word Len

-- limitGuessCount: Forces guessCount to be less than 15
limitGuessCount :: Int -> Int
limitGuessCount x
  | x > 15    = 15
  | otherwise = x
  
-- matchesLen: Compares - Returns True if word is a specified length
matchesLen :: Int -> String -> Bool
matchesLen wordLen str
  | wordLen == (length str) = True
  | otherwise               = False
  
--getFileName: Parses fileName from user's input arguments
getFileName :: [String] -> String
getFileName (fileName:xs) = fileName

-- getWordLen: Parses WordLen from user's input arguments
getWordLen :: [String] -> Int
getWordLen (x:wordLen:xs)
  | (isADigit wordLen) = read wordLen
  | otherwise = 0

-- getGuessCount: Parses GuessCount from user's input arguments
getGuessCount :: [String] -> Int
getGuessCount (x:y:guessCount:xs)
  | (isADigit guessCount) = read guessCount
  | otherwise = 0

--Function to check if an input character can be a digit
isADigit :: String -> Bool
isADigit [] = True
isADigit (x:xs)
  | isDigit x = isADigit xs
  | otherwise = False
