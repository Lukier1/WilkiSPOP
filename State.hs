module State where

import Map

--definicja ruchu
data Turn = WolfTurn | SheepsTurn

--definicja stanu
data State = State Board Turn

--funkcja zwracająca jakośc stanu
quality :: State -> Float
quality (State [] _) = 0
quality (State xs _) = quality' (snd position) (degreesofFreedom position xs)
    where position = findWolf xs

--f pomocnicza
quality':: Int->Int->Float--quality' Wysyokość Stopnieswobody
quality' a b= (-3)*(fromIntegral a) + (fromIntegral b)

--funkcja zwracająca stopnie swobody danej pozycji (ile ma możliwości ruchu)
degreesofFreedom:: Position -> Board -> Int
degreesofFreedom (x,y) bs = (degreeofFreedom leftUp bs) + (degreeofFreedom rightUp bs) + (degreeofFreedom leftDown bs)+ (degreeofFreedom rightDown bs)
    where leftUp = (x-1,y+1);rightUp=(x+1,y+1);leftDown=(x-1,y-1);rightDown=(x+1,y-1)

--funkcja zwraca 1 jeżeli istnieje możliwośc przejścia na dane pole
degreeofFreedom:: Position ->Board -> Int
degreeofFreedom (x,y) bs    | (fieldExists (x,y))&&(isEmpty (bs!!(getInd (x,y)))) = 1 
                            | otherwise = 0

--czy pole znajduje sie na planczy
fieldExists:: Position-> Bool
fieldExists (x,y)   | (1<=x)&&(x<=mapSize)&&(1<=y)&&(y<=mapSize)= True
                    | otherwise = False

