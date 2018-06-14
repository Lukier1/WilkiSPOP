module InputOutput where

import System.IO
import Map

-- moduł obsługujący operacje I/O

saveGameToFile gameBoard = do
 putStrLn "Proszę podać nazwę pliku do zapisania gry:" 
 filePath <- getLine
 saveToFile gameBoard filePath
 putStrLn $ "Zapisano grę do pliku: " ++ filePath ++ "\n"

saveToFile :: Board -> FilePath -> IO ()
saveToFile gameBoard filePath = writeFile filePath (drawMap gameBoard)

loadGameFromFile = do
 putStrLn "Proszę podać nazwę pliku z zapisaną grą do wczytania:" 
 savedFile <- getLine
 --loadedFile <- loadFromFile savedFile
 putStrLn $ "Gra z pliku: " ++ savedFile 
 printFromSavedFile savedFile
 putStrLn "Poprawnie wczytano grę z pliku.\n"

{-
loadFromFile :: String -> IO Board
loadFromFile savedFile =
 withFile savedFile ReadMode (\handle -> do
 fileContent <- hGetContents handle
 readIO fileContent)
-}

-- tymczasowe 'śmieci": do wyrzucenia później ...

printFromSavedFile :: String -> IO ()
printFromSavedFile savedFile =
 withFile savedFile ReadMode (\handle -> do
 fileContent <- hGetContents handle
 putStrLn fileContent)

{-
instance Board where
 readIO ::  Board -> [Field]
-}