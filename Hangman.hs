-- Keegan Laporte and Noah Nagy
-- CS231 02L Lab8
-- Main.hs
module Main where
import System.Environment
import System.Directory
import System.IO
import Data.Char
import PlayEvilHangman

-- EvilHangman:
-- Program Description
-- Usage: "./Hangman dictFileName WordLength GuessCount [Optional] -n"
-- This file: Parses user inputs and initializes game
-- Support Files: playEvilHangman.hs will perform the game

main = do
  -- Process: Check arguments, and if all input is good, play evilHangman in the proper mode
  -- 1. Open Dictionary
  args <- getArgs
  fileExists <- doesFileExist $ getFileName args
  if (not fileExists)
  
  then -- Bad Dictionary File -- Tell User
    putStr "Dictionary file does not exist\n"
    
  else do -- Good Dictionary File
    dictHandle  <- openFile (getFileName args) ReadMode
    dictContent <- hGetContents dictHandle
    let dictionary = words dictContent
    
    -- 2. Check argument Validity
    let validArgsMsg   = "" --isValidArgs args dictionary
    if (validArgsMsg)  == ""
    
    then do -- Good Arguments --Play Evil Hangman!!
      let wordLen      = getWordLen args
      let shortenDic   = shortenDictionary dictionary wordLen
      let debugMode = checkDebugMode args
      if ((getGuessCount args) > 15)
      then do
        let guessCount = 15
        putStr "Number of guesses set to maximum of 15\n"
        playEvilHangman shortenDic wordLen guessCount debugMode --Play Evil Hangman!
      else do
        let guessCount = (getGuessCount args)
        playEvilHangman shortenDic wordLen guessCount debugMode --Play Evil Hangman!
      
    else do -- Bad Arguments -- Tell User
      putStr $ validArgsMsg ++ "Usage: ./Hangman dictionary name length of word number of guesses\n"
    
    

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
  | badWordLen (getWordLen args) dictionary = "Error: A word of that length does not exist in the selected dictionary"
  | (getGuessCount args) < 1                = "Error: You must select a number of guesses between 1 and 26"
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
getWordLen (x:wordLen:xs) = read wordLen

-- getGuessCount: Parses GuessCount from user's input arguments
getGuessCount :: [String] -> Int
getGuessCount (x:y:guessCount:xs) = read guessCount
