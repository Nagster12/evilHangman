-- Keegan Laporte and Noah Nagy
-- CS231 02L Lab8
-- PlayEvilHangman.hs

module PlayEvilHangman where
import System.Environment
import System.IO
import Data.Char
import Control.Monad (when)
import Data.List

-- playEvilHangman: The game begins! with the user selected options we will play evil hangman
playEvilHangman :: [String] -> Int -> Int -> Bool -> IO ()
playEvilHangman dictionary wordLength guessCount isDebugMode = do
  -- Initial Conditions of variables
  let familyPattern = [1,3]
  let guessedLetters = []
  let hangmanWord = []
  -- Iterate game state till game is over
  recursiveHangman dictionary wordLength guessCount isDebugMode guessedLetters familyPattern hangmanWord
  
  
recursiveHangman :: [String] -> Int -> Int -> Bool -> [Char] -> [Int] -> String -> IO ()
recursiveHangman dictionary wordLength guessCount isDebugMode guessedLetters familyPattern hangmanWord = do
  -- Show user current Game State Info
   -- step 1
  putStrLn ("Guesses Left: " ++ show guessCount)
  -- step 2
  putStrLn ("Letters Guessed: " ++ show guessedLetters)
  -- step 3
  putStrLn ("Word so far: " ++ showHangmanWord hangmanWord guessedLetters familyPattern)
  putStrLn "Guess?:"
  -- step 4
  debugPrint (currFamilyPatternCount familyPattern) isDebugMode
  -- step 5:
  -- Request user to guess for a letter
  eof <- hIsEOF stdin
  -- Check for eof
  if eof 
    then return ()
    else do
      -- Get that letter as inputChar
      inputLine <- hGetLine stdin
      let inputChar = head inputLine
      if (((not.isAlpha) inputChar) || (elem inputChar guessedLetters)) -- the user's char has to be an unused letter
        then do -- Get the user to enter a new input
        putStrLn "\nError: Invalid Guess: Please choose a unique letter"
        recursiveHangman dictionary wordLength guessCount isDebugMode guessedLetters familyPattern hangmanWord
        else do
        -- Step 6
        -- Partion dictionary with the inputChar (TODO: Noah)
        -- Create a newDict *important for step 9*
        let newDict = ["new", "dictionary"]
        -- Step 7
        -- Update familyPattern (TODO: Noah)
        -- step 8 update guessCount when input char is not in the dictionary
        let newGuessCount = if (not (charInDict inputChar newDict))
                            then guessCount - 1
                            else guessCount
        -- step 9
        when (newGuessCount == 0) (putStrLn "You're out of Guesses. Game Over")
        -- step 10 
        putStrLn "got to this point."

-- Helper Functions:
--TODO FINISH ShowHangman
showHangmanWord :: String -> [Char] -> [Int] -> String
showHangmanWord hangmanWord guessedletters familyPattern = "fake hangman word"

-- currDictFamilyCount (I think it is finished)
currFamilyPatternCount :: [Int] -> Int
currFamilyPatternCount currPattern = length currPattern

debugPrint :: Int -> Bool -> IO ()
debugPrint patternCount isDebugMode = do
  if isDebugMode then putStr (show patternCount) else (return ())
  
charInDict :: Char -> [String] -> Bool
charInDict _ [] = False
charInDict char (x:xs) 
  | elem char x = True
  | otherwise   = charInDict char xs 

{-

	To find families:
		1. Check the letter input
		2. Check placement of letter in words
		3. Combine words that have same placement

-}

--Finds the words that belong to a family
findFamily :: [String] -> [Int] -> String -> [String]
findFamily [] wordIndeces inputLetter = []
findFamily (x:xs) wordIndeces inputLetter
   | (findLetterIndices x inputLetter) == wordIndeces = x : findFamily xs wordIndeces inputLetter
   | otherwise = findFamily xs wordIndeces inputLetter

--Find the words with contained letter in the dictionary
findWords :: [String] -> Char -> [String]
findWords [] guessLetter = []
findWords [""] guessLetter = []
findWords (x:xs) guessLetter =
   filter ((\ x str -> elem x str) guessLetter) (x:xs)

--Find the words without contained letter in the dictionary
findNoWords :: [String] -> Char -> [String]
findNoWords [] guessLetter = []
findNoWords [""] guessLetter = []
findNoWords (x:xs) guessLetter = 
   filter (not . (\ x str -> elem x str) guessLetter) (x:xs)

--Find family in dictionary
findLetterIndices :: String -> String -> [Int]
findLetterIndices input letter = findIndices (`elem` letter) input
