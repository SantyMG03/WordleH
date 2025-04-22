module Wordle where

import Data.List (elemIndices, (\\))
import qualified Data.Map as M
import Data.List (groupBy, sortOn)
import Data.Function (on)

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
newTry guess secret = zip guess clues
  where
    -- Paso 1: detectamos los aciertos exactos (C)
    exacts = zipWith (\g s -> if g == s then Just C else Nothing) guess secret
    -- Paso 2: contamos letras de la palabra secreta que no se han emparejado
    filteredGuess = [g | (g, e) <- zip guess exacts, e == Nothing]
    filteredSecret = [s | (g, s) <- zip guess secret, g /= s]
    remaining = countLetters filteredSecret
    -- Paso 3: para el resto, ponemos I si hay en remaining, si no N
    fill [] [] _ = []
    fill (g:gs) (Just C : es) rem = Just C : fill gs es rem
    fill (g:gs) (Nothing : es) rem =
      if M.findWithDefault 0 g rem > 0
      then Just I : fill gs es (M.adjust (\x -> x - 1) g rem)
      else Just N : fill gs es rem

    clues = map (\(Just c) -> c) $ fill guess exacts remaining

countLetters :: (Ord a) => [a] -> M.Map a Int
countLetters = foldr (\c m -> M.insertWith (+) c 1 m) M.empty

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
updateLS old new = foldr update old (bestClues new)
  where
    update (ch, clue) acc =
      case lookup ch acc of
        Nothing   -> (ch, clue) : acc
        Just prev -> (ch, maxClue clue prev) : filter ((/= ch) . fst) acc

-- Agrupa todas las pistas nuevas y se queda con la mejor para cada letra
bestClues :: Try -> Try
bestClues try = map best $ groupBy ((==) `on` fst) $ sortOn fst try
  where
    best group@((ch, _):_) = (ch, foldr1 maxClue (map snd group))
    best [] = error "Grupo vacío inesperado"

maxClue :: Clue -> Clue -> Clue
maxClue C _ = C
maxClue _ C = C
maxClue I _ = I
maxClue _ I = I
maxClue N N = N