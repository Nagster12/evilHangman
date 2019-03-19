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

-- playEvilHangman: The game begins! with the user selected options we will play evil hangman
playEvilHangman :: [String] -> Int -> Int -> Bool -> IO ()
playEvilHangman dictionary wordLength guessCount isDebugMode = do
  -- Initial Conditions of variables
  let familyPattern = [1,3]
  let guessedLetters = []
  let hangmanWord = makeDashList wordLength
  -- Iterate game state till game is over
  recursiveHangman dictionary wordLength guessCount isDebugMode guessedLetters familyPattern hangmanWord
  
makeDashList :: Int -> String
makeDashList 0 = []
makeDashList n
  | n /= 0    = '-':makeDashList (n-1)
  | otherwise = []
  
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
      -- Get user selected letter as inputChar
  inputLine <- hGetLine stdin
  let inputChar = if (inputLine == []) then ' ' else (head inputLine)
  if (((not.isAlpha) inputChar) || (elem inputChar guessedLetters)) -- the user's char has to be an unused letter
  then do -- Get the user to enter a new input
    putStrLn "\nError: Invalid Guess: Please choose a unique letter"
    recursiveHangman dictionary wordLength guessCount isDebugMode guessedLetters familyPattern hangmanWord 
  else do
    let newGuessedLetters = inputChar:guessedLetters
    -- Step 6
    -- Update familyPattern
    let newIndices = findAllIndices dictionary [inputChar]
    let newFamilyPattern = findCommon newIndices
    -- Step 7
    -- Partion dictionary with the inputChar --Creates a newDict *important for step 9*
    let newDict = findFamily dictionary newFamilyPattern [inputChar]
    -- step 8 update guessCount when input char is not in the dictionary
    let newGuessCount = if (not (charInDict inputChar newDict))
                        then guessCount - 1
                        else guessCount
    let newHangmanWord = makeHangmanWord hangmanWord inputChar newFamilyPattern
    -- step 9 Check if end of game
    if (length dictionary == 1) && not(elem '-' newHangmanWord) then (putStrLn ("You win! The word was: " ++ newHangmanWord))
    else do
      if (newGuessCount == 0)
    then do
      putStrLn "You're out of Guesses. Game Over"
      putStrLn ("The correct word was: " ++ (head newDict))
      --TODO more end game cases such as Winning and other checks for Loses
      -- step 10 Need to go back with updated parameters
    else recursiveHangman newDict wordLength newGuessCount isDebugMode newGuessedLetters newFamilyPattern newHangmanWord

-- Helper Functions:
--makeHangmanWord: arguments - hangmanWord inputChar familyPattern
makeHangmanWord :: String -> Char -> [Int] -> String
makeHangmanWord hangmanWord guessedLetter []            = hangmanWord
makeHangmanWord []          guessedLetter familyPattern = []
makeHangmanWord (x:xs)      guessedLetter familyPattern
  | (head familyPattern == 0) = guessedLetter: makeHangmanWord xs guessedLetter (map (\x -> x - 1) (tail familyPattern))
  | otherwise                 = x:makeHangmanWord xs guessedLetter (map (\x -> x - 1) familyPattern)
  
removeFirstLetters :: [String] -> [String]
removeFirstLetters ((y:ys):xs) = [str| str <- ys]:removeFirstLetters xs

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

--Function to find the most common index
findCommon xs = head . maximumBy (comparing length) . group . sort $ xs

{-
	Finds all indecies of character in current dictionary/family
	Inputs the current family/dictionary and the letter of interest
	Outputs a list of all the indices in
    the family
-}
findAllIndices :: [String] -> String -> [[Int]]
findAllIndices [] inputLetter = []
findAllIndices (x:xs) inputLetter = (findWordIndices x inputLetter) : findAllIndices xs inputLetter

--Finds the words that belong to a family based on indecies
findFamily :: [String] -> [Int] -> String -> [String]
findFamily [] wordIndices inputLetter = []
findFamily (x:xs) wordIndices inputLetter
   | (findWordIndices x inputLetter) == wordIndices = x : findFamily xs wordIndices inputLetter
   | otherwise = findFamily xs wordIndices inputLetter

--Find indices of guessed letter in current word
findWordIndices :: String -> String -> [Int]
findWordIndices input letter = findIndices (`elem` letter) input
