-- Keegan Laporte and Noah Nagy
-- CS231 02L Lab8
-- PlayEvilHangman.hs

module PlayEvilHangman where
import System.Environment
import System.IO
import Data.Char
import Control.Monad (when)
import Data.List
import Data.Ord

{-

	playEvilHangman: The game begins! with the user selected options
			 we will play evil hangman
	Inputs: Dictionary, guessed word length, number of guesses, check
			if we print out length of dictionary

-}
playEvilHangman :: [String] -> Int -> Int -> Bool -> IO ()
playEvilHangman dictionary wordLength guessCount isDebugMode = do
  -- Initial Conditions of variables
  let familyPattern = []
  let guessedLetters = []
  let hangmanWord = makeDashList wordLength
  -- Iterate game state till game is over
  recursiveHangman dictionary wordLength guessCount isDebugMode guessedLetters familyPattern hangmanWord

--Function that creates the placeholders for letters based on word length
makeDashList :: Int -> String
makeDashList 0 = []
makeDashList n
  | n /= 0    = '-':makeDashList (n-1)
  | otherwise = []

{-
	Function that runs the game
	Inputs: Dictionary, length of the word, number of guesses,
		checks if we print family size, the current guessed
		letters, the current family pattern
	Outputs:
		Prints needed data to the console
-}
recursiveHangman :: [String] -> Int -> Int -> Bool -> [Char] -> [Int] -> String -> IO ()
recursiveHangman dictionary wordLength guessCount isDebugMode guessedLetters familyPattern hangmanWord = do
  -- Show user current Game State Info
   -- step 1
  putStrLn ("Guesses Left: " ++ show guessCount)
  -- step 2
  putStrLn ("Letters Guessed: " ++ show guessedLetters)
  -- step 3
  putStrLn ("Word so far: " ++ show hangmanWord)
  -- step 4
  when isDebugMode (putStrLn $ "Number of possible words: " ++ show (length dictionary)) -- only prints if in debug mode: "-n"
  -- step 5:
  -- Request user to guess for a letter
      -- Get user selected letter as inputLetter
  inputLine <- hGetLine stdin
  let inputLetter = if (inputLine == []) then ' ' else (head inputLine)
  if (((not.isAlpha) inputLetter) || (elem inputLetter guessedLetters)) -- the user's char has to be an unused letter
  then do -- Get the user to enter a new input
    putStrLn "\nError: Invalid Guess: Please choose a unique letter"
    recursiveHangman dictionary wordLength guessCount isDebugMode guessedLetters familyPattern hangmanWord 
  else do
    let newGuessedLetters = inputLetter:guessedLetters
    -- Step 6
    -- Update familyPattern
    let newIndices = findAllPatterns dictionary [inputLetter]
    let newFamilyPattern = findCommon newIndices
    -- Step 7
    -- Partion dictionary with the inputLetter --Creates a newDict *important for step 9*
    let newDict = findFamily dictionary newFamilyPattern [inputLetter]
    -- step 8 update guessCount when input char is not in the dictionary
    --		update the hangman word to print
    let newGuessCount = if (not (charInDict inputLetter newDict))
                        then guessCount - 1
                        else guessCount
    let newHangmanWord = makeHangmanWord hangmanWord inputLetter newFamilyPattern
    -- step 9 Check if end of game
    if (length dictionary == 1) && not(elem '-' newHangmanWord) then (putStrLn ("You win! The word was: " ++ newHangmanWord))
    else do
      if (newGuessCount == 0)
       then do
        putStrLn "You're out of Guesses. Game Over"
        putStrLn ("The correct word was: " ++ (head newDict))
        -- step 10 Need to go back with updated parameters
       else recursiveHangman newDict wordLength newGuessCount isDebugMode newGuessedLetters newFamilyPattern newHangmanWord

-- Helper Functions:
--makeHangmanWord: arguments - hangmanWord inputLetter familyPattern
makeHangmanWord :: String -> Char -> [Int] -> String
makeHangmanWord hangmanWord guessedLetter []            = hangmanWord
makeHangmanWord []          guessedLetter familyPattern = []
makeHangmanWord (x:xs)      guessedLetter familyPattern
  | (head familyPattern == 0) = guessedLetter: makeHangmanWord xs guessedLetter (map (\x -> x - 1) (tail familyPattern))
  | otherwise                 = x:makeHangmanWord xs guessedLetter (map (\x -> x - 1) familyPattern)
 
--Function to check if letter is in current dictionary
charInDict :: Char -> [String] -> Bool
charInDict _ [] = False
charInDict char (x:xs) 
  | elem char x = True
  | otherwise   = charInDict char xs 

--Function to find the most common index
findCommon :: [[Int]] -> [Int] 
findCommon xs = head . maximumBy (comparing length) . group . sort $ xs

{-
	Finds all patterns of character in current dictionary/family
	Inputs: the current family/dictionary and the letter of interest
	Outputs: a list of all the patterns in the family

-}
findAllPatterns :: [String] -> String -> [[Int]]
findAllPatterns [] inputLetter = []
findAllPatterns (x:xs) inputLetter = (findWordPatterns x inputLetter) : findAllPatterns xs inputLetter

--Finds the words that belong to a family based on patterns
findFamily :: [String] -> [Int] -> String -> [String]
findFamily [] wordPattern inputLetter = []
findFamily (x:xs) wordPattern inputLetter
   | (findWordPatterns x inputLetter) == wordPattern = x : findFamily xs wordPattern inputLetter
   | otherwise = findFamily xs wordPattern inputLetter

--Find pattern of guessed letter in current word
findWordPatterns :: String -> String -> [Int]
findWordPatterns input letter = findIndices (`elem` letter) input
