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

-- Wypluwa string zawierajcy mape
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

--Przeliczanie indeks -> poyzcja na planszy
getPos :: Int -> Position  
getPos n = ((n-1) `mod` mapSize + 1, 1 + quot (n-1) mapSize)

--Przeliczanie pozycja -> indeks na planszy
getInd :: Position -> Int    
getInd (x, y) = (x-1)+(y-1)*mapSize + 1


--Sprawdzanie zawartosci pól
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



--funkcja zwraca pozycje owcy o podanym indeksie
findSheep :: Board -> Int -> Position
findSheep board index = case findIndex  (\x -> isSheep x index)  board of
                            Just ind ->  getPos (ind+1)  
                            Nothing -> error $ "No sheep for index: " ++ show index


--funkcja zwraca pozycję wilka
findWolf:: Board -> (Int, Int)
findWolf [] = error "Empty"
findWolf ys = getPos (findWolf' ys 1)

--f pomocniacza
findWolf':: Board -> Int -> Int 
findWolf' [] _ = error "End"
findWolf' (x:xs) i  | (isWolf x) = i
                    | otherwise = findWolf' xs (i+1)

changeField :: Board -> Position -> Field -> Board
changeField board pos field = 
    let ind = getInd pos 
    in take (ind-1) board ++ [field] ++ drop (ind) board

--Funkcje slużace do poruszania, nie sprawdzaja poprawnosci wprowadzonych danych
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


--Testowe funkcje
testTable :: Board
testTable = [Wolf, Empty, Empty, (Sheep 2), Empty, Empty, Empty, Empty, (Sheep 1)]

startMap = generateStartMap

mapTest = do   
            putStrLn $ drawMap generateStartMap
            putStrLn $ show $ findSheep generateStartMap 3     
            