module EightOff where
  
  import System.Random
  
  data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Enum, Eq, Ord)
  
  data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Enum, Eq, Ord)
  
  type Card = (Pip,Suit)
  
  type Deck = [Card] -- List of all the cards
  
  type EOBoard = [(Foundations,Columns,Reserve)]
  
  type Foundations = (Deck,Deck,Deck,Deck)
  -- foundations = []
  
  type Columns = (Deck,Deck,Deck,Deck,Deck)
  -- columns = []
  
  type Reserve = (Card,Card,Card,Card,Card,Card,Card,Card)
  -- reserve = []
  
  
  
  
  
  -- type EOBoard = []
  
  pack :: Deck
  -- List comprehention: combindes the pips and suits into unique pairs to form a deck of 52 cards.
  pack = [(pip,suit) | pip <- [Ace ..], suit <- [Clubs ..]]
  
  sCard :: Card -> Card
  sCard (a,b) = (succ a,b)
  
  pCard :: Card -> Card
  pCard (a,b) = (pred a,b)
  
  -- !!!!! NOTE: Test this by using map isAce pack !!!!!
  isAce :: Card -> Bool
  isAce (a,_) = a == Ace
  
  isKing :: Card -> Bool
  isKing (a,_) = a == King