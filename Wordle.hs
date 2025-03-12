module Wordle where

data Clue = C | I | N | U
  deriving (Eq,Ord,Read,Show)

type Try = [(Char,Clue)]

letters :: [Char]
letters = "abcdefghijklmnopqrstuvwxyz"

validLetters :: String -> Bool
validLetters word = all (`elem` letters) word

newTry :: String -> String -> Try
newTry secret guess = zipWith evaluate guess [0..]
  where
    evaluate g i 
    | g == secret !! i (g, C)
    | g `elem` secret = (g, N)
    | otherwise = (g, I)

initialLS :: Try
initialLS = []

updateLS :: Try -> Try -> Try
updateLS oldTry newTry = newTry

