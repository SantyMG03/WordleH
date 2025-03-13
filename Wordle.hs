module Wordle where

import Data.List (elemIndices, (\\))

data Clue = C | I | N | U
  deriving (Eq, Ord, Read, Show)

type Try = [(Char, Clue)]

letters :: [Char]
letters = "abcdefghijklmnopqrstuvwxyz"

-- Verifica si una palabra es válida (usa solo letras permitidas y tiene la longitud correcta)
validLetters :: String -> Bool
validLetters word = all (`elem` letters) word

-- Genera un nuevo intento comparando con la palabra secreta
newTry :: String -> String -> Try
newTry guess correct = zip guess (map getClue (zip guess correct))
  where
    getClue (g, c)
      | g == c = C
      | g `elem` correct = I
      | otherwise = N

-- Intento inicial sin información
initialLS :: Try
initialLS = []

-- Actualiza la información de los intentos previos con uno nuevo
updateLS :: Try -> Try -> Try
updateLS old new = foldr update old new
  where
    update (ch, clue) acc = case lookup ch acc of 
      Just C -> acc
      _ -> (ch, clue) : filter ((/= ch) . fst) acc