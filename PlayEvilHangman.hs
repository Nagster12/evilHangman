-- Keegan Laporte and Noah Nagy
-- CS231 02L Lab8
-- PlayEvilHangman.hs

module PlayEvilHangman where
import System.Environment
import System.IO
import Data.Char
import Data.List

--playEvilHangman:: [String] -> Int -> Int -> Int
playEvilHangman dictionary wordLength guessCount = do
  putStrLn "inside EvilHangman"
  --putStr (show wordLength ++ show guessCount ++ show dictionary) -- DEBUG: Shows all inputs



{-
	To find all families
		1. Check input letter
		2. Find all indices possible
		3. Store into list
-}

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


