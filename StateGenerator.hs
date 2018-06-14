module StateGenerator where

import State
import Map

---------------------------------------------------------------------
--Funkja generujaca wszystkie mozliwe stany pochodzace od danego stanu
stateGenerateFromState :: State -> [State]
stateGenerateFromState (State board turn) = case turn of
                                                WolfTurn -> generateStateWolfTurn board
                                                SheepsTurn ->  generateStateSheepsTurn board 
-----------
--Funckje pomocnicze
--Funkcja generujaca wszystkie stany dla owiec
generateStateSheepsTurn :: Board -> [State]
generateStateSheepsTurn board = generateStateForSheep board 1

--Generowanie stanow dla owiec z podanym i mniejszym indeksem
generateStateForSheep :: Board -> Int -> [State]
generateStateForSheep board 5 = []
generateStateForSheep board index = 
    let (oX, oY) = findSheep board index
        rightPos = (oX+1, oY+1)
        leftPos =  (oX-1, oY+1)
        moveLeftBoard = moveWithCheck board (oX, oY) leftPos (Sheep index)
        moveRightBoard = moveWithCheck board (oX, oY) rightPos (Sheep index)
        genState board  | null board = [] 
                        | otherwise = [(State board WolfTurn)]
    in (genState moveLeftBoard) ++ (genState moveRightBoard) ++ generateStateForSheep board (index+1)

--Funkcja generujaca wszystkie stany dla wilka
generateStateWolfTurn :: Board -> [State]
generateStateWolfTurn board = 
    let (oX, oY) = findWolf board 
        rightDownPos = (oX+1, oY+1)
        leftDownPos =  (oX-1, oY+1)
        rightUpPos = (oX+1, oY-1)
        leftUpPos =  (oX-1, oY-1)
        boardRD = moveWithCheck board (oX, oY) rightDownPos Wolf
        boardLD = moveWithCheck board (oX, oY) leftDownPos Wolf
        boardRU = moveWithCheck board (oX, oY) leftUpPos Wolf
        boardLU = moveWithCheck board (oX, oY) rightUpPos Wolf
        genState board  | null board = [] 
                        | otherwise = [(State board SheepsTurn)]
    in (genState boardRD) ++ (genState boardLD) ++ (genState boardRU) ++ (genState boardLU)
    
--Funkcja poruszajaca gdy istnieje taka mozliwosc
moveWithCheck :: Board -> Position -> Position -> Field -> Board
moveWithCheck board oldPos newPos field =
    if degreeofFreedom newPos board == 1 then moveFromTo board oldPos newPos field else []  


turnPrint turn =  case turn of
    WolfTurn -> "Wilcza tura"
    SheepsTurn -> "Owcza tura"
    
statePrint (State board turn) = drawMap board ++ "\nTura:" ++ turnPrint turn