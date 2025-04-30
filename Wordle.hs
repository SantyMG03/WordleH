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
validLetters :: String -> Bool
validLetters word = all (`elem` letters) word 

-- Genera un nuevo intento comparando con la palabra secreta
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

-- Helper function to determine the priority of a clue
cluePriority :: Clue -> Int
cluePriority C = 3 -- Posicion correcta
cluePriority I = 2 -- Posicion incorrecta pero esta en la palabra
cluePriority N = 1 -- No esta en la palabra
cluePriority U = 0 -- Desconocido

-- Actualiza la información de los intentos previos con uno nuevo
updateLS :: Try -> Try -> Try
updateLS oldLS newTry = M.toList $ M.unionWith chooseBest combinedMap initialMap
  where
    -- Initial state with all letters as Unknown
    initialMap = M.fromList $ zip letters (repeat U)
    -- Combine old and new tries
    combinedPairs = oldLS ++ newTry
    -- Create map from combined tries, resolving duplicates with best clue
    combinedMap = M.fromListWith chooseBest combinedPairs
    -- Function to choose the best clue based on priority
    chooseBest :: Clue -> Clue -> Clue
    chooseBest c1 c2 = if cluePriority c1 >= cluePriority c2 then c1 else c2
    -- M.unionWith merges the two maps, using chooseBest when a key exists in both.
    -- This ensures the initial 'U' is overridden by a more informative clue.
