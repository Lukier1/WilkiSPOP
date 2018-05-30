data Field = 
            Wolf |
            Sheep Int |
            Empty

--Stała Wielkość mapy
mapSize = 8


--Utils
toInt :: Float -> Int
toInt x = round x

drawField :: Field -> String
drawField Wolf = "x"
drawField (Sheep n) = show n
drawField Empty = "-"

drawRow :: [Field] -> Int -> Int -> String
drawRow [] _ _ = "\n"
drawRow xs _ 0 = ""
drawRow xs 0 y = "\n" ++ drawRow xs mapSize (y-1)
drawRow (x:xs) n y = drawField x ++ drawRow xs (n-1) y

drawMap :: [Field] -> String
drawMap xs = drawRow xs mapSize mapSize 
 
generateStartMap :: [Field]
generateStartMap = let rGenerateStartMap n  | n == 2 || n == 4 || n == 6 || n == 8  = (Sheep (quot n 2) ):rGenerateStartMap (n+1) 
                                            | n == 57 = (Wolf):rGenerateStartMap (n+1) 
                                            | otherwise = (Empty):rGenerateStartMap (n+1)
                                            | n > (mapSize*mapSize) = []
                   in rGenerateStartMap (1)     

getPos :: Int -> (Int, Int)    
getPos n = ((n-1) `mod` mapSize + 1, 1 + quot (n-1) mapSize)

getInd :: (Int, Int) -> Int    
getInd (x, y) = (x-1)+(y-1)*mapSize + 1

--checkFieldIfEmpty :: (Int, Int) -> Bool
--checkFieldIfEmpty (x, y)  = let rGenerateStartMap n  

testTable = [Wolf, Empty, Empty, (Sheep 2), Empty, Empty, Empty, Empty, (Sheep 1)]

mapTest = do   
            putStr $ drawMap testTable
            