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
  let validateMsg = checkArgs args
  let mode        = checkDebugMode args
  if validateMsg /= "" putStr validateMsg -- An Error has occurred
  else --TODO Parse all the arguments and pass it into the play function
  (playHangman dictionary wordLength guessCount mode)
  
checkArgs :: [String] -> String
checkArgs args
  | badFileName   (getFileName   args) = "Error: The dictionary does not exist"
  | badWordLength (getWordLength args) = "Error: A word of that length does not exist in my Dictionary"
  | badGuessCount (getGuessCount args) = "Error: You must select a number of guesses larger than 1 and less than 26"
  | otherwise = ""

checkDebugMode [String] -> Bool
checkDebugMode args
  | (length args) == 4 && (last args) == "-n" = True
  | (length args) == 3                        = False

playHangman:: [String] -> Int -> Int -> Bool
playHangman args = True