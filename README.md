-- Task B:

import Cards 
import Wrapper 
import Test.QuickCheck
import System.Random

aCard1 :: Card 
aCard1 = (Card (Numeric 5) Hearts)

aCard2 :: Card 
aCard2 = (Card Jack Spades)

aHand :: Hand 
aHand = Add aCard1 (Add aCard2 Empty)

empty :: Hand 
empty = Empty

valueRank :: Rank -> Integer 
valueRank ( Numeric x) = x 
valueRank ( Jack ) = 10 
valueRank ( Queen ) = 10 
valueRank ( King ) = 10 
valueRank ( Ace ) = 11

valueCard :: Card -> Integer 
valueCard ( Card r s ) = valueRank r

numberOfAces :: Hand -> Integer 
numberOfAces Empty = 0 
numberOfAces (Add c h) = if (rank c == Ace) 
                        then 1 + numberOfAces h 
                        else numberOfAces h

value1 :: Hand -> Integer 
value1 Empty = 0 
value1 (Add c h) = (valueCard c) + (value1 h)

value :: Hand -> Integer 
value h | value1 h > 21 = value1 h - (numberOfAces h * 10) 
        | otherwise = value1 h

gameOver :: Hand -> Bool 
gameOver h = value1 h > 21

winner :: Hand -> Hand -> Player 
winner handGuest handBank | gameOver(handGuest) = Bank 
                          | gameOver(handBank) = Guest 
                          | value1(handGuest) > value1(handBank) = Guest         
                          | value1(handBank) > value1(handGuest)= Bank 
                          | value1(handGuest) == value1(handBank)= Bank

--Task C:

(<+) :: Hand -> Hand -> Hand 
Empty <+ hand2 = hand2 
(Add card hand1) <+ hand2 = Add card (hand1 <+ hand2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool 
prop_size_onTopOf p1 p2 = (size p1) + (size p2) == size (p1 <+ p2)

--Task D:

handSuit :: Suit -> Hand 
handSuit s = (Add (Card Ace s) (Add (Card King s) (Add (Card Queen s) (Add (Card Jack s) Empty))))

handSuit' = [ Numeric x | x <- [2..10]] 

handSuit'' :: [Card] -> Hand
handSuit'' [] =  Empty
handSuit'' (x:xs) =  Add x (handSuit'' xs)


fullDeck :: Hand

fullDeck = (handSuit Spades) <+ (handSuit Clubs) <+ (handSuit Diamonds) <+ (handSuit Hearts)

--Task E: 

draw:: Hand -> Hand -> (Hand, Hand) 
draw Empty _ = error ("draw:No cards.") 
-- Draw funktionen lÃ¥ter oss flytta ett kort frÃ¥n deck till h2. 
-- Add c deck Ã¶ppnar kortleken sÃ¥ att vi kan ta det Ã¶versta kortet frÃ¥n deck 
-- och lÃ¤gga det i h2. Add c h2 lÃ¤gger till kortet i h2. 
draw (Add c deck) h2 = (deck, (Add c h2))

-- Task F: 

playBank :: Hand -> Hand 
playBank deck = playBank' deck Empty

playBank' :: Hand -> Hand -> Hand 
playBank' deck bankHand = if value bankHand < 16 
                          then playBank' deck' bankHand 
                          else bankHand 
  where (deck', bankHand') = draw deck bankHand


-- Task G:

shuffle :: StdGen -> Hand -> Hand
shuffle g deck | deck == Empty = deck
               | otherwise = ( Add (pickCard deck n') (removeCard deck n'))
                     where (n',g') = randomR (1,52) g 



removeCard :: Hand -> Integer -> Hand
removeCard ( Add card hand) n | n == 1 = hand
                             | otherwise = ( Add card (removeCard hand (n-1)))
 
 
pickCard :: Hand -> Integer -> Card
pickCard ( Add card hand) n | n == 1 = card
                           | otherwise = pickCard hand (n-1)




-- Task H :

implementation = Interface
  {  iEmpty     = empty
  ,  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation
