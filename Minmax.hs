module Minmax where

import State
import StateGenerator
import Map
import Data.List
import Data.Maybe

pinv = 9999999
minv = -9999999
depthGlob = 4

--funkcja zwracająca najlepsze posunięcie dla wilka
getNextWolfsMove :: State -> State
getNextWolfsMove (State board turn) | length states > 1 = mapMax (alphaBetha depthGlob minv pinv) states 
                                    | otherwise = states!!0
    where states = stateGenerateFromState (State board turn)

--algorytm alfa beta
alphaBetha :: Int -> Float -> Float -> State -> Float
alphaBetha depth alpha betha (State board turn) | (depth <= 0) || endState (State board turn) = quality (State board turn) 
                                                | isWolfTurn turn = alphaBethaWolf depth alpha betha states
                                                | otherwise = alphaBethaSheep depth alpha betha states
                                            where states = stateGenerateFromState (State board turn)
                                                                           

alphaBethaWolf :: Int -> Float -> Float -> [State] -> Float
alphaBethaWolf depth alpha betha [] = alpha
alphaBethaWolf depth alpha betha (x:xs) | newAlpha >= betha = betha
                                        | otherwise = alphaBethaWolf depth newAlpha betha xs
                                    where newAlpha = max alpha (alphaBetha (depth-1) alpha betha x) 
                               
alphaBethaSheep :: Int -> Float -> Float -> [State] -> Float
alphaBethaSheep depth alpha betha [] = betha
alphaBethaSheep depth alpha betha (x:xs) | alpha >= newBetha = alpha
                                         | otherwise = alphaBethaSheep depth alpha newBetha xs
                                    where newBetha = min betha (alphaBetha (depth-1) alpha betha x)                             

isWolfTurn::Turn -> Bool
isWolfTurn x = case x of
            WolfTurn -> True
            _-> False


mapMax :: (t -> Float) -> [t] -> t
mapMax f (x:xs) = 
    let  list = map f (x:xs)
         maxNr = maximum list
    in (x:xs)!!(fromJust (findIndex (==maxNr) list))




--Tests
minmaxtest = statePrint (getNextWolfsMove (State wilkMapTest WolfTurn))

wilkMapTest =   let rGenerateStartMap n     | n == 11 || n == 4 || n == 6 || n == 8  = (Sheep (quot n 2) ):rGenerateStartMap (n+1) 
                                            | n == 9 = (Wolf):rGenerateStartMap (n+1) 
                                            | otherwise = (Empty):rGenerateStartMap (n+1)
                                            | n > (mapSize*mapSize) = []
                in rGenerateStartMap (1)

startState = (State wilkMapTest WolfTurn)

statesWolf = stateGenerateFromState startState

statesSheep1 = stateGenerateFromState (statesWolf!!0)
statesSheep2 = stateGenerateFromState (statesWolf!!1)

albe = \x y -> alphaBetha x minv pinv y