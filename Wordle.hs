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
newTry secret guess
  | length guess /= length secret = error "Las palabras deben tener la misma longitud"
  | otherwise = zip guess (evaluate guess secret)
  where
    -- Paso 1: Marcar correctamente las letras en la posición correcta (C)
    markCorrect :: String -> String -> [Clue]
    markCorrect g s = zipWith (\g s -> if g == s then C else U) g s
      where (g, s) = (guess, secret)
    
    -- Paso 2: Marcar las letras incorrectas (I) y las que están en otra posición (N)
    markRest :: String -> String -> [Clue] -> [Clue]
    markRest g s cs = zipWith (\g c -> if c == C then C else if g `elem` remaining then N else I)
                              g cs
      where remaining = s \ map fst (filter (\x -> snd x == C) (zip g cs))
    
    evaluate g s = markRest g s (markCorrect g s)


-- Intento inicial sin información
initialLS :: Try
initialLS = []

-- Actualiza la información de los intentos previos con uno nuevo
updateLS :: Try -> Try -> Try
updateLS old new = new