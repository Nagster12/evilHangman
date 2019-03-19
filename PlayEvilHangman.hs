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
  eof <- hIsEOF stdin
  -- Check for eof
  if eof 
    then return ()
    else do
      -- Get user selected letter as inputChar
      inputLine <- hGetLine stdin
      let inputChar = head inputLine
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
        if (length dictionary == 1) then (putStrLn "You win!")
        else do
          if (newGuessCount == 0) then (putStrLn "You're out of Guesses. Game Over")
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


{-
	Finds the largest family to use 
	Inputs the list of indices, the current family list,
		the letter we are interested in, and the length
		of each family at each indice. Start at length of 0
	Ouputs the family that has the most number of words
-}
findLargestFamily :: [[Int]] -> [String] -> String -> [String]
--findLargsetFamily [] currFamily inputLetter currMost = []
--findLargestFamily (x) currFamily inputLetter = 
findLargestFamily (x:y:xs) [a] inputLetter = [a]
findLargestFamily (x:y:xs) currFamily inputLetter
   | (length thisFamily) > (length nextFamily) = findLargestFamily (y:xs) currFamily inputLetter 
   | otherwise = findLargestFamily (x:xs) currFamily inputLetter
   where thisFamily = (findFamily currFamily x inputLetter)
         nextFamily = (findFamily currFamily y inputLetter)

findCommon xs = head . maximumBy (comparing length) . group . sort $ xs

{-
	Finds the indice for the family being currently used 
	Inputs the list of indices, the current family list,
		the letter we are interested in, and the length
		of each family at each indice. Start at length of 0
	Ouputs the indices list of the family to be used
-}
findFamilyIndices :: [[Int]] -> [String] -> String -> Int-> [Int]
findFamilyIndices [] currFamily inputLetter currMost = []
findFamilyIndices (x:y:xs) [] inputLetter currMost = []
findFamilyIndices (x:y:xs) currFamily inputLetter currMost  
   | (length nextFamily) > currMost = findFamilyIndices xs currFamily inputLetter (length nextFamily)
   | otherwise = x
   where nextFamily = (findFamily currFamily x inputLetter)

{-
	Finds all indecies of character in current dictionary/family
	Inputs the current family/dictionary and the letter of interest
	Outputs a list of all the indices in the family
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


