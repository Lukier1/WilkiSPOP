module InputOutput where

import System.IO
import Map

-- moduł obsługujący operacje I/O

saveGameToFile gameBoard = do
 putStrLn "Proszę podać nazwę pliku do zapisania gry:" 
 filePath <- getLine
 saveToFile gameBoard filePath
 putStrLn "Zapisano grę do pliku."

saveToFile :: Board -> FilePath -> IO ()
saveToFile gameBoard filePath = writeFile filePath (drawMap gameBoard)
