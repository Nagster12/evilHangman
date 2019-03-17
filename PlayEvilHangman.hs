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
  
  
recursiveHangman :: [String] -> Int -> Int -> Bool -> [Char] -> [Int] -> IO ()
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
	--TODO: Keegan, update guessedLetters with new character to view
        -- Step 6
        -- Update familyPattern (TODO: Noah)
        let newIndices = findAllIndices dictionary [inputChar]
        let newFamilyPattern = findFamilyIndices newIndices dictionary [inputChar] 0
        -- Step 7
        -- Partion dictionary with the inputChar (TODO: Noah)
        -- Create a newDict *important for step 9*
        let newDict = findLargestFamily newIndices dictionary [inputChar] 0
        let lengthDict = length newDict
        -- step 8 update guessCount when input char is not in the dictionary
        let newGuessCount = if (not (charInDict inputChar newDict))
                            then guessCount - 1
                            else guessCount
        -- step 9 Check if end of game
        when (newGuessCount == 0) (putStrLn "You're out of Guesses. Game Over")
	
	--Below needs "-n" implementation, debug to see number of words possible
        putStrLn ("Number of possible words: " ++ show lengthDict) 
	
        -- step 10 Need to go back with updated parameters
        --recursiveHangman newDict wordLength newGuessCount isDebugMode guessedLetters newFamilyPattern
        putStrLn "got to this point."

-- Helper Functions:
--TODO FINISH ShowHangman
showHangmanWord :: String -> [Char] -> [Int] -> String
showHangmanWord hangmanWord guessedletters familyPattern = show guessedletters

-- currDictFamilyCount (I think it is finished)
currFamilyPatternCount :: [Int] -> Int
currFamilyPatternCount currPattern = length currPattern

debugPrint :: Int -> Bool -> IO ()
debugPrint patternCount isDebugMode = do
  if isDebugMode then putStrLn (show patternCount) else (return ())
  
charInDict :: Char -> [String] -> Bool
charInDict _ [] = False
charInDict char (x:xs) 
  | elem char x = True
  | otherwise   = charInDict char xs 


{-
	Finds the largest family to use 
	Inputs the list of indices, the current family list,
		the letter we are interested in, and the length
		of each family at each indice. Start at length of 0
	Ouputs the family that has the most number of words
-}
findLargestFamily :: [[Int]] -> [String] -> String -> Int-> [String]
findLargsetFamily [] currFamily inputLetter currMost = []
findLargestFamily (x:xs) [] inputLetter currMost = []
findLargestFamily (x:xs) currFamily inputLetter currMost  
   | (length $ findFamily currFamily x inputLetter) > currMost = findLargestFamily xs currFamily inputLetter (length (findFamily currFamily x inputLetter))
   | (length $ findFamily currFamily x inputLetter) < currMost = findLargestFamily xs currFamily inputLetter currMost
   | otherwise = findFamily currFamily x inputLetter

{-
	Finds the indice for the family being currently used 
	Inputs the list of indices, the current family list,
		the letter we are interested in, and the length
		of each family at each indice. Start at length of 0
	Ouputs the indicie list of the family to be used
-}
findFamilyIndices :: [[Int]] -> [String] -> String -> Int-> [Int]
findFamilyIndices [] currFamily inputLetter currMost = []
findFamilyIndices (x:xs) [] inputLetter currMost = []
findFamilyIndices (x:xs) currFamily inputLetter currMost  
   | (length $ findFamily currFamily x inputLetter) > currMost = findFamilyIndices xs currFamily inputLetter (length (findFamily currFamily x inputLetter))
   | (length $ findFamily currFamily x inputLetter) < currMost = findFamilyIndices xs currFamily inputLetter currMost
   | otherwise = x

{-
	Finds all indecies of character in current dictionary/family
	Inputs the current family/dictionary and the letter of interest
	Outputs a list of all the indices in the family
-}
findAllIndices :: [String] -> String -> [[Int]]
findAllIndices [] inputLetter = []
findAllIndices (x:xs) inputLetter = (findLetterIndices x inputLetter) : findAllIndices xs inputLetter

--Finds the words that belong to a family based on indecies
findFamily :: [String] -> [Int] -> String -> [String]
findFamily [] wordIndeces inputLetter = []
findFamily (x:xs) wordIndeces inputLetter
   | (findLetterIndices x inputLetter) == wordIndeces = x : findFamily xs wordIndeces inputLetter
   | otherwise = findFamily xs wordIndeces inputLetter

--Find indices of guessed letter in current word
findLetterIndices :: String -> String -> [Int]
findLetterIndices input letter = findIndices (`elem` letter) input


