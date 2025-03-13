module Wordle where

import Data.List (elemIndices, (\\))

data Clue = C | I | N | U
  deriving (Eq, Ord, Read, Show)

type Try = [(Char, Clue)]

letters :: [Char]
letters = "abcdefghijklmnopqrstuvwxyz"

-- Verifica si una palabra es válida (usa solo letras permitidas y tiene la longitud correcta)
-- all funciona con una funcion que devuelve T/F y una lista de elementos -> devuelve true si
--      todos lo elementos cumplen con la condicion
-- elem funciona con un elemento a buscar y un lista, devuelve T si el elemnto esta, F en otro caso
-- Para cada letra de word comprueba que este en letters
validLetters :: String -> Bool
validLetters word = all (`elem` letters) word 

-- Genera un nuevo intento comparando con la palabra secreta
-- zip ([a],[b]) devuelve tuplas (a,b)
-- map recibe una funcion y una lista sobre la que aplicar la funcion
-- newTry forma tuplas de (guess, Clue) donde guess es cada una de las letras de la palabra intento
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
-- foldr recibe una funcion, un acumulador y la lista a reducir, devolviendo la lista reducida
-- lookup comprueba si ch ya esta en acc, si ch ya era C -> no lo cambia, si no lo reemplaza 
--      con una nueva pista
-- filter aplica una funcion a una lista, en este caso: toma el primer elmento de la tupla (char, Clue)
--      con fst y verifica que dicha letra sea diferente de ch. Eliminara cualquier aparicion previa de 
--      ch en acc
updateLS :: Try -> Try -> Try
updateLS old new = foldr update old new
  where
    update (ch, clue) acc = case lookup ch acc of 
      Just C -> acc
      _ -> (ch, clue) : filter ((/= ch) . fst) acc