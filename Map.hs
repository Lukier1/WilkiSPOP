module Map where

import Data.List
    
data Field = 
            Wolf |
            Sheep Int |
            Empty

--Stała Wielkość mapy
mapSize = 8

type Board = [Field]
type Position = (Int, Int)

loadMap :: String -> Board
loadMap (x:xs) = case x of 
                'x' -> (Wolf):loadMap xs 
                '1' -> (Sheep 1):loadMap xs
                '2' -> (Sheep 2):loadMap xs 
                '3' -> (Sheep 3):loadMap xs 
                '4' -> (Sheep 4):loadMap xs 
                '-' -> (Empty):loadMap xs
                '\n'-> loadMap xs
                '\0'-> loadMap xs
                ' ' -> loadMap xs
                _ -> error "Nieprawidlowa mapa"
loadMap [] = []

-- Wypluwa string zawierajacy mape
drawMap :: [Field] -> String
drawMap xs = drawRow xs mapSize mapSize 

--funkcje pomocnicze dla draw
drawField :: Field -> String
drawField Wolf = "x"
drawField (Sheep n) = show n
drawField Empty = "-"

drawRow :: [Field] -> Int -> Int -> String
drawRow [] _ _ = "\n"
drawRow xs _ 0 = ""
drawRow xs 0 y = "\n" ++ drawRow xs mapSize (y-1)
drawRow (x:xs) n y = drawField x ++ drawRow xs (n-1) y

--generowanie pustej planszy
generateStartMap :: [Field]
generateStartMap = let rGenerateStartMap n  | n == 2 || n == 4 || n == 6 || n == 8  = (Sheep (quot n 2) ):rGenerateStartMap (n+1) 
                                            | n == 57 = (Wolf):rGenerateStartMap (n+1) 
                                            | otherwise = (Empty):rGenerateStartMap (n+1)
                                            | n > (mapSize*mapSize) = []
                   in rGenerateStartMap (1)     

--Przeliczanie indeksu -> pozycja na planszy
getPos :: Int -> Position  
getPos n = ((n-1) `mod` mapSize + 1, 1 + quot (n-1) mapSize)

--Przeliczanie pozycji -> indeks na planszy
getInd :: Position -> Int    
getInd (x, y) = (x-1)+(y-1)*mapSize + 1


--Sprawdzanie zawartości pól
isEmpty::Field -> Bool
isEmpty x = case x of
            Empty -> True
            _-> False
             
isWolf::Field -> Bool
isWolf x = case x of
            Wolf -> True
            _ ->False 

isSheep::Field -> Int -> Bool
isSheep x index = case x of 
                Sheep n -> n == index 
                _ -> False



--funkcja zwraca pozycję owcy o podanym indeksie
findSheep :: Board -> Int -> Position
findSheep board index = case findIndex  (\x -> isSheep x index)  board of
                            Just ind ->  getPos (ind+1)  
                            Nothing -> error $ "No sheep for index: " ++ show index


--funkcja zwraca pozycję wilka
findWolf:: Board -> (Int, Int)
findWolf [] = error "Empty"
findWolf ys = getPos (findWolf' ys 1)

--f pomocnicza
findWolf':: Board -> Int -> Int 
findWolf' [] _ = error "End"
findWolf' (x:xs) i  | (isWolf x) = i
                    | otherwise = findWolf' xs (i+1)

changeField :: Board -> Position -> Field -> Board
changeField board pos field = 
    let ind = getInd pos 
    in take (ind-1) board ++ [field] ++ drop (ind) board

--Funkcje służace do poruszania, nie sprawdzają poprawności wprowadzonych danych
moveFromTo :: Board -> Position -> Position -> Field -> Board
moveFromTo board (oX, oY) (nX, nY) field = 
    let cleanBoard = changeField board (oX, oY) Empty
        movedBoard = changeField cleanBoard (nX, nY) field
    in movedBoard

moveSheep :: Board -> Int -> (Int,Int) -> Board
moveSheep board index (x,y) = 
    let (oX, oY) = findSheep board index
        newPos = (oX+x, oY+y)
    in moveFromTo board (oX, oY) newPos (Sheep index)



