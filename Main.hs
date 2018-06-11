module Main where 

import GameOptions
{-
 Moduł uruchamiający program. 

 Kompilacja: ghc -o Gra Main.hs -outputdir 'parsed'

 Uruchomienie: ./Gra
-}

main = do
 putStrLn "\nWitamy w grze Wilk i Owce. Zapraszamy do gry!"
 
 runMainMenu