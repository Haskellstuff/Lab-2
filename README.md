Lab-2
=====

BlackJack


module BlackJack where

--import Cards
--import Wrapper

-- Suit är Spades elr Hearts etc. "data" definierar en ny datatyp vilket är Suit. Det finns 4 olika värden i Suit. Show gör så att ett värde "a" blir en string. Alltså så att när man skriver in spades så printar den ut spades t.ex.
data Suit = Spades | Hearts | Diamonds | Clubs
  deriving Show

-- Vi gör en ny datatyp som vi kallar Rank. Rank har 5 värden i form av bl.a, Numeric n där n är en siffra (använder Integer för siffror).  
data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving Show

data Colour = Red | Black
  deriving Show

colour :: Suit -> Colour
colour Spades = Black
colour Clubs  = Black
colour s      = Red

-- 
data Card = Card Rank Suit
