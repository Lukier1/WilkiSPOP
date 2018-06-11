module GameOptions where

import Map
import State
import StateGenerator

{-
 Moduł odpowiedzialny za logikę gry oraz interakcje z użytkownikiem.
-}

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

{-
Poniższe wydmuszki tylko zaznaczają logikę gry. Po ustaleniu poszczególnych kroków (patrz kwestie), wgram implementację.
-}
startNewGame = do
 putStr "Wybrano opcję rozpoczęcia nowej gry. Przykładowa plansza początkowa:\n"
 -- Kwestia 1: tu moim zdaniem trzeba dać użytkownikowi opcję wskazania miejsc owiec na planszy. @Łukasz: 1. Czy miejsce początkowe wilka jest stałe? 2. Jak proponujesz umożliwić rozmieszczenie owiec?
 mapTest
 -- Kwestia 2: Tu wchodzi w grę algorytm. Wilk wykonuje swój ruch, a potem użytkownik decyduje gdzie ruszyć owce.
 runGameMenu
 -- Kwestia 3: Po każdym ruchu (owca + wilk) proponuję wyświetlać opcje gry. Moim zdaniem tak będzie zgodnie z wymaganiami. Co wy na to?

stopGame = do
 putStr "Zatrzymano grę. By wznowić naciśnij dowolny klawisz.\n"
 userKeyboardHit <- getLine
 resumeGame

resumeGame = do
 putStr "Powrócono do gry.\n"
 -- Kwestia 4: tu chcę wrzucić opcję 
 runGameMenu -- do wyrzucenia. Pozostawiłem tylko by zachować ciągłość po uruchomieniu programu

saveGame = do
 putStr "Zapisano grę do pliku ...\n"
 -- tu będzie zapisanie aktualnego stanu gry do pliku
 resumeGame

loadSavedGame = do
 putStr "Wczytano zapisaną grę ...\n"
 -- tu będzie wczytanie wcześniej zapisanej gry z pliku po podaniu ścieżki
 resumeGame -- do wyrzucenia. Wyjaśnienie jak wyżej

exitGame = do
 putStr "Dziękujemy za wspólną grę. Do zobaczenia!\n"
 return()

wrongOptionMessage = "Wybrano nieprawidłową opcję z menu. Proszę spróbować jeszcze raz.\n"
