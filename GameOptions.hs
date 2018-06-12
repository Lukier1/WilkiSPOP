module GameOptions where

import InputOutput
import Map
import State
import StateGenerator

--Moduł odpowiedzialny za logikę gry oraz interakcje z użytkownikiem.

displayInitialMenu = "\nWybierz jedną z opcji:\n\
 \1. Rozpocznij nową grę\n\
 \2. Wczytaj zapisaną grę\n\
 \3. Wyjdź z programu\n"

displayGameMenu = "Wybierz jedną z opcji:\n\ 
 \1. Zatrzymaj grę\n\ 
 \2. Zapisz grę do pliku\n\ 
 \3. Wróć do głównego menu programu (uwaga! kasuje obecny stan gry)\n\ 
 \4. Wyjdź z gry\n"

runMainMenu = do 
 putStrLn displayInitialMenu
 selectedOption <- getLine
 case selectedOption of
  "1" -> startNewGame
  "2" -> loadSavedGame
  "3" -> exitGame
  _ -> do
   putStr wrongOptionMessage
   runMainMenu

runGameMenu = do 
 putStr displayGameMenu
 selectedGameOption <- getLine
 case selectedGameOption of
  "1" -> stopGame
  "2" -> saveGame
  "3" -> runMainMenu
  "4" -> exitGame
  _ -> do
   putStr wrongOptionMessage
   runGameMenu

startNewGame = do
 putStr "Wybrano opcję rozpoczęcia nowej gry. Przykładowa plansza początkowa:\n"
 mapTest
 runGameMenu

stopGame = do
 putStr "Zatrzymano grę. By wznowić naciśnij dowolny klawisz.\n"
 userKeyboardHit <- getLine
 resumeGame

resumeGame = do
 putStrLn "By powrócić do gry, proszę nacisnąć klawisz"
 getLine
 runGameMenu -- Tu zamiast menu gry wrzucimy powrócenie do porzedniego stanu jak już będzie dodany algorytm gry.

saveGame = do
 saveGameToFile testTable
 resumeGame

loadSavedGame = do
 putStr "Wczytano zapisaną grę ...\n"
 -- tu będzie wczytanie wcześniej zapisanej gry z pliku po podaniu ścieżki
 resumeGame -- do wyrzucenia. Wyjaśnienie jak wyżej

exitGame = do
 putStr "Dziękujemy za wspólną grę. Do zobaczenia!\n"
 return()

wrongOptionMessage = "Wybrano nieprawidłową opcję z menu. Proszę spróbować jeszcze raz.\n"
