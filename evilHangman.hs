-- Keegan Laporte and Noah Nagy
-- CS231 02L Lab8
-- evilHangman.hs
module Main where
import System.Environment
import System.IO
import Data.Char


main = do
  args <- getArgs
  -- Check argument Validity
  checkArgs args

  
checkArgs :: [String] -> String
checkArgs args
  | validWordLength (getWordLength args) = "Error: A word of that length does not exist in my Dictionary"
  | validGuess (getGuessCount args) == 4 && (last args) == "-n" = putStr (playHangmanDebug args)
  | (length args) == 3                        = putStr (playHangmanNormal args)
  | otherwise = "error"
  else putStr "error"
  --[dictFile, wordLength, guessCount, debugN]
  --putStr inputLength
  
playHangmanNormal:: [String] -> String
playHangmanNormal args = "Normal"

playHangmanDebug:: [String] -> String
playHangmanDebug args = "Debug"