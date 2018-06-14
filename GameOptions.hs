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
  "2" -> loadSavedGame --2 saveGame
  "3" -> exitGame
  _ -> do
   putStr wrongOptionMessage
   runMainMenu

runGameMenu stateGame = do 
 putStr displayGameMenu
 putStrLn "\nAktualny stan gry to:"
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
        "p" -> moveIfPossible stateGame sheepInd (1, 1)
        "l" -> moveIfPossible stateGame sheepInd (-1, 1)
        _ -> runGameMenu stateGame

--porusz owce jesli to mozliwe w innym przypadku wywala blad i wraca do stanu wyboru owcy
--po poruszeniu owcy uruchamia minmax i AI porusza wilka, a nastepnie przechodzi do stanu wyboru owcy
moveIfPossible (State board turn) sheepInd (x, y) =  
    let (oX, oY) = findSheep board sheepInd
        newPos = (oX+x, oY+y)
        moveBoard = moveWithCheck board (oX, oY) newPos (Sheep sheepInd)
        newState = (State moveBoard WolfTurn)
        foundState = getNextWolfsMove newState
    in do case moveBoard of [] ->   do  putStr "Nie mozna poruszyc owcy na to pole!\n" -- w tym przypadku plansza nie zostalo stworzona, wiec zly ruch
                                        runGameMenu (State board turn)
                            _ -> do if endSheepWon newState then 
                                        endGame newState
                                    else
                                        do  if endWolfWon foundState then 
                                                endGame foundState
                                            else
                                                runGameMenu foundState  
                                  
startNewGame = do
 putStr "Wybrano opcję rozpoczęcia nowej gry."
 runGameMenu startGameState

stopGame lastState = do
 putStrLn "Zatrzymano grę. By wznowić naciśnij enter."
 userKeyboardHit <- getLine
 resumeGame lastState

resumeGame lastState = do
 putStrLn "Powrócono do gry."
 runGameMenu lastState

saveGame (State map turn) = do
 saveGameToFile map
 resumeGame (State map turn)

loadSavedGame = do
 loadGameFromFile
 resumeGame startGameState -- do zmiany startGameState na aktualny stan gry z pliku (załadować lastState z pliku)
{-
loadSavedGame2 stateGame = do
 loadGameFromFile
 resumeGame stateGame
-}
exitGame = do
 putStr "Dziękujemy za wspólną grę. Do zobaczenia!\n"
 return()

endGame (State board turn) = case turn of 
                        WolfTurn -> do  putStr "Wygrałeś!\n"
                                        runMainMenu
                        SheepsTurn -> do putStr "Przegrałeś!\n"
                                         runMainMenu

wrongOptionMessage = "Wybrano nieprawidłową opcję z menu. Proszę spróbować jeszcze raz.\n"
