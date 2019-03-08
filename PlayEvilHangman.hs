-- Keegan Laporte and Noah Nagy
-- CS231 02L Lab8
-- PlayEvilHangman.hs

module PlayEvilHangman where
import System.Environment
import System.IO
import Data.Char

playEvilHangman dictionary wordLength guessCount debugMode = do
  putStr "it works"
  --putStr (show wordLength ++ show guessCount ++ show dictionary) -- DEBUG: Shows all inputs