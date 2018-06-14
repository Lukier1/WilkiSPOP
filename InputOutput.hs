module InputOutput where

import System.IO
import System.Directory
import Map
--import GameOptions

-- moduł obsługujący operacje I/O

saveGameToFile gameBoard = do
 putStrLn "Proszę podać nazwę pliku do zapisania gry:" 
 filePath <- getLine
 saveToFile gameBoard filePath
 putStrLn $ "Zapisano grę do pliku: " ++ filePath ++ "\n"

saveToFile :: Board -> FilePath -> IO ()
saveToFile gameBoard filePath = writeFile filePath (drawMap gameBoard)


loadGameFromFile :: IO(Board)
loadGameFromFile = do
    putStrLn "Proszę podać nazwę pliku z zapisaną grą do wczytania:" 
    savedFile <- getLine
    fileCheck <- doesFileExist savedFile 
    if (fileCheck)
    then do 
        putStrLn $ "Gra z pliku: " ++ savedFile 
        content <- readFile savedFile
        putStrLn "Poprawnie wczytano grę z pliku.\n"
        return (loadMap content)
    else do
        putStrLn $ "Nie znaleziono pliku " ++ savedFile ++ ". Proszę spróbować jeszcze raz.\n"
        loadGameFromFile