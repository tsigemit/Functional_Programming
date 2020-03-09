module BlackJack where
import Cards
import RunGame
import System.Random

-- PART A0
{--
size hand2
= size (Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty))
= 1 + size (Add (Card Jack Spades) Empty)
= 1 + 1 + size(Empty)
= 1 + 1 + 0
= 2
--}

-- PART A1 
-- returns an empty hand
empty :: Hand
empty = Empty

-- PART A2 
value :: Hand -> Integer
value hand = 
    if initialValue hand <= 21
        then initialValue hand
        else initialValue hand - 10 * numberOfAces hand

-- returns the value of a hand using 11 for aces
initialValue :: Hand -> Integer    
initialValue Empty = 0
initialValue (Add card hand) = valueCard card + value hand

-- returns the number of aces, 
-- needed to recompute the value of a hand when a player is bust
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) = 
    if rank card == Ace
        then 1 + numberOfAces hand
        else numberOfAces hand

-- returns the value of a single card
valueCard :: Card -> Integer
-- the value of a hand only depends on its rank
valueCard (Card rank _) = valueRank rank

-- returns the value of a given card rank
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric n) = n
valueRank _ = 10    -- everything else, i.e. J, Q, K

-- PART A3
-- returns true if the value of a hand exceeds 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- PART A4
-- returns the winner of the game given two (sorted) hands
winner :: Hand -> Hand -> Player
winner guestHand bankHand | not (gameOver guestHand) && value guestHand > value bankHand = Guest
                          | not (gameOver guestHand) && gameOver bankHand = Guest
                          | otherwise = Bank


-- PART B1
-- h1 <+ h2 = h1-h2 or h1+h2
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand  = hand
(<+) hand  Empty = hand
(<+) (Add card firstHand) secondHand = Add card (firstHand <+ secondHand)

-- QuickCheck proporty for Associativity of hands 
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- QuickCheck proporty for the size of hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2) 

-- PART B2
fullDeck :: Hand
fullDeck =  addToSuit Hearts <+ addToSuit Spades <+
            addToSuit Diamonds <+ addToSuit Clubs

-- Create list of cards for the given suit
addToSuit :: Suit -> Hand
addToSuit suit = foldr Add Empty
                        ([Card (Numeric n) suit | n <- [2 .. 10]] ++

                        [Card rank suit | rank <- [Jack, Queen, King, Ace]])

-- PART B3
-- to draw one card from the deck and put on the hand
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ =  error "draw: The deck is empty."
draw (Add card rest) hand = (rest, Add card hand)

-- PART B4
-- given a deck, it returns the bank's final hand
-- (the bank starts with an empty hand)
playBank :: Hand -> Hand
playBank deck = playBank' deck empty

-- helper function for playBank
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand
    | value bankHand >= 16 = bankHand
    | otherwise = playBank' deck' bankHand'
    where (deck', bankHand') = draw deck bankHand


-- PART B5
-- Given a random generator and a hand, shuffle the cards
-- and returns the shuffled hand
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g fromHand = Add card (shuffle g' hand)
      where
            (cardIndex, g') = randomR (0, size fromHand -1) g
            (card, hand) = removeCard fromHand cardIndex

-- Removes the n:th card from a deck
removeCard :: Hand -> Integer -> (Card, Hand)
removeCard _ n | n<0 = error "remove  Card: negative index"
removeCard hand n = removeCard' Empty hand n

removeCard' :: Hand -> Hand -> Integer -> (Card, Hand)
removeCard' topPart Empty n = error "removeCard: index exceeds hand size"
removeCard' topPart (Add c bottomPart) 0 =
                          (c, topPart `addReverse` bottomPart)
removeCard' topPart (Add c bottomPart) n =
                          removeCard' (Add c topPart) bottomPart (n-1)

-- Adds one hand on top of the other in reverse order
addReverse :: Hand -> Hand -> Hand
addReverse h1 h2 = reverseHand h1 <+ h2

-- Reverses a hand
reverseHand :: Hand -> Hand
reverseHand h = reverseHand' h Empty
    
reverseHand' :: Hand -> Hand -> Hand
reverseHand' Empty h' = h'
reverseHand' (Add c r) h' = reverseHand' r (Add c h')

-- Function to check if a card belongs to a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Check if a card is in a deck before it has been shuffled
-- then it should be in the deck afterwards
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h ==
                               c `belongsTo` shuffle g h

-- After shuffle the size of the cards in the hand should be the same
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffle g hand)

 -- PART B6
-- interface
implementation :: Interface
implementation = Interface
    { iEmpty    = empty
    , iFullDeck = fullDeck
    , iValue    = value
    , iGameOver = gameOver
    , iWinner   = winner
    , iDraw     = draw
    , iPlayBank = playBank
    , iShuffle  = shuffle
  }
main :: IO ()
main = runGame implementation

