module Main where

import Wordle

type TestNewTry = (String,String,Try)
type TestUpdateLS = (String,Try)

iFile1 = "TestTLP2025Estudiantes-newTry.txt"
iFile2 = "TestTLP2025Estudiantes-updateLS.txt"

-- FUNCIÓN QUE LEE EL FICHERO DE TESTS DE LA FUNCIÓN newTry
readNewTryTests :: FilePath -> IO [String]
readNewTryTests f = do { putStrLn "Cargando tests de la función newTry";
                         fcontent <- (readFile f);
                         return (lines fcontent);
                       }

-- FUNCIÓN QUE EJECUTA LOS TESTS LEÍDOS (SI HAY TESTS)
executeTestsNewTry [] = putStrLn "¡Error! La lista de tests de la función newTry es vacía."
executeTestsNewTry xs = checkNewTry (map readAsTest xs) True
  where readAsTest s = (read s)::TestNewTry

-- FUNCIÓN QUE COMPRUEBA LOS TESTS DE LA FUNCIÓN newTry
checkNewTry :: [TestNewTry] -> Bool -> IO ()
checkNewTry [] result = do { putStr "Fin de los tests de la función newTry, ";
                             if result then putStrLn "todos los tests han sido superados." 
                                       else putStrLn "ha habido tests con fallos."
                           }
checkNewTry ((intento,oculta,salida):resto) result
  | coincide  = checkNewTry resto result
  | otherwise = do { putStrLn ( "-Error en el test: (\""++intento++"\",\"" ++ oculta ++ "\")" );
                     checkNewTry resto False
                   }
    where coincide = (newTry intento oculta) == salida

-- FUNCIÓN QUE LEE LOS TESTS DEL FICHERO DE TESTS DE LA FUNCIÓN updateLS
readUpdateLSTests :: FilePath -> IO [String]
readUpdateLSTests f = do { putStrLn "Cargando tests de la función updateLS";
                           fcontent <- (readFile f);
                           return (lines fcontent);
                         }

-- FUNCIÓN QUE EJECUTA LOS TESTS LEÍDOS (SI HAY TESTS)
executeTestsUpdateLS [] = putStrLn "¡Error! No se ha especificado la palabra oculta en la primera línea."
executeTestsUpdateLS [x] = putStrLn "¡Error! La lista de tests de la función updateLS es vacía."
executeTestsUpdateLS (oculta:tests) = checkUpdateLS oculta (map readAsTest tests) initialLS True
  where readAsTest s = (read s)::TestUpdateLS

-- FUNCIÓN QUE COMPRUEBA LOS TESTS DE LA FUNCIÓN updateLS
checkUpdateLS _ [] _ result = do { putStr "Fin de los tests de la función updateLS, ";
                                   if result then putStrLn "todos los tests han sido superados." 
                                             else putStrLn "ha habido tests con fallos."
                                 }
checkUpdateLS oculta ((intento,correctLS):resto) statusLS result
  | coincide  = checkUpdateLS oculta resto newStatusLS result
  | otherwise = do { putStrLn ( "-Error en el test: \""++intento++"\"." );
                     checkUpdateLS oculta resto correctLS False
                   }
    where coincide = newStatusLS == correctLS
          newStatusLS = updateLS statusLS (newTry intento oculta)

-- FUNCIÓN PRINCIPAL
main :: IO ()
main = do { testsNT <- readNewTryTests iFile1;
            executeTestsNewTry testsNT;
            testsULS <- readUpdateLSTests iFile2;
            executeTestsUpdateLS testsULS;
            
          }

