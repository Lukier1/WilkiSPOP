module GameOptions where

import InputOutput
import Map
import State
import StateGenerator
import Minmax

--Moduł odpowiedzialny za logikę gry oraz interakcje z użytkownikiem.

displayInitialMenu = "\nWybierz jedną z opcji:\n\
 \1. Rozpocznij nową grę\n\
 \2. Wczytaj zapisaną grę\n\
 \3. Wyjdź z programu\n"

displayGameMenu = "Wybierz jedną z opcji:\n\ 
 \1 .. 4 - wybór owcy do poruszenia\n\ 
 \S - Zatrzymaj grę\n\ 
 \W - Zapisz grę do pliku\n\ 
 \M - Wróć do głównego menu programu (uwaga! kasuje obecny stan gry)\n\ 
 \Q - Wyjdź z gry\n"

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

runGameMenu stateGame = do 
 putStr displayGameMenu
 putStr $ drawStateMap $ stateGame 
 selectedGameOption <- getLine
 case selectedGameOption of
  "1" -> moveSheepMenu stateGame 1
  "2" -> moveSheepMenu stateGame 2
  "3" -> moveSheepMenu stateGame 3
  "4" -> moveSheepMenu stateGame 4
  "S" -> stopGame stateGame
  "W" -> saveGame stateGame
  "M" -> runMainMenu
  "Q" -> exitGame
  _ -> do
   putStr wrongOptionMessage
   runGameMenu stateGame

-- Stan w ktorym sterowana jest pojednycza owca
moveSheepMenu stateGame sheepInd = do 
    putStr $ "Move sheep " ++ show sheepInd ++ "\n\tP - ruch w prawo\n\tL - ruch w lewo\n\tother input - powroc do wyboru owcy\n"  
    selectedMove <- getLine
    case selectedMove of 
        "P" -> moveIfPossible stateGame sheepInd (1, 1)
        "L" -> moveIfPossible stateGame sheepInd (-1, 1)
        _ -> runGameMenu stateGame

--porusz owce jesli to mozliwe w innym przypadku wywala blad i wraca do stanu wyboru owcy
--po poruszeniu owcy uruchamia minmax i AI porusza wilka, a nastepniep przechodzi do stanu wyboru owcy
moveIfPossible (State board turn) sheepInd (x, y) =  
    let (oX, oY) = findSheep board sheepInd
        newPos = (oX+x, oY+y)
        moveBoard = moveWithCheck board (oX, oY) newPos (Sheep sheepInd)
    in do case moveBoard of [] ->   do  putStr "Nie mozna poruszyc owcy na to pole!\n" -- w tym przyapdku plansza nie zostalo stworzona, wiec zly ruch
                                        runGameMenu (State board turn)
                            _ -> runGameMenu $ getNextWolfsMove (State moveBoard WolfTurn) 
        
startNewGame = do
 putStr "Wybrano opcję rozpoczęcia nowej gry. Przykładowa plansza początkowa:\n"
 runGameMenu startGameState

stopGame lastState = do
 putStr "Zatrzymano grę. By wznowić naciśnij dowolny klawisz.\n"
 userKeyboardHit <- getLine
 resumeGame lastState

resumeGame lastState = do
 putStrLn "By powrócić do gry, proszę nacisnąć klawisz"
 getLine
 runGameMenu lastState -- Tu zamiast menu gry wrzucimy powrócenie do porzedniego stanu jak już będzie dodany algorytm gry.

saveGame (State map turn) = do
 saveGameToFile map
 resumeGame (State map turn)

loadSavedGame = do
 putStr "Wczytano zapisaną grę ...\n"
 -- tu będzie wczytanie wcześniej zapisanej gry z pliku po podaniu ścieżki
 resumeGame startGameState -- do wyrzucenia. Wyjaśnienie jak wyżej

exitGame = do
 putStr "Dziękujemy za wspólną grę. Do zobaczenia!\n"
 return()

wrongOptionMessage = "Wybrano nieprawidłową opcję z menu. Proszę spróbować jeszcze raz.\n"
