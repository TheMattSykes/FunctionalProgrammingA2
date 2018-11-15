module EightOff where
  
  import MergeSort
  import System.Random
  import Data.List
  
  {- 1. Haskell Datatypes -}
  
  data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Enum, Eq, Ord)
  
  data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Enum, Eq, Ord)
  
  type Card = (Pip,Suit)
  
  type Deck = [Card] -- List of all the cards
  
  type EOBoard = (Foundations,Columns,Reserve)
  
  -- Sequence builder from Ace
  type Foundations = [Deck]
  
  -- Other cards
  type Columns = [Deck] -- !!!!! Change type later
  
  -- Hold one card
  type Reserve = [Card]
  
  
  
  {- 2. Haskell constants and utilities -}
  
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
  
  
  
  {- 3. Shuffle Function -}
  
  randomList = take 56 (randoms (mkStdGen 64) :: [Int])
  zippedList = zip pack randomList
  sortedList = mergesort (\(_,n1) (_,n2)->n1<n2) zippedList
  
  shuffle = map fst sortedList
  
  
  
  {- 4. eODeal -}
  
  eOBoard :: EOBoard
  eOBoard = ([],col,res)
  
  res :: Reserve
  res = take 4 shuffle
  
  col :: Columns
  col = columnGenerator (drop 4 shuffle)
  
  columnGenerator :: Eq a => [a] -> [[a]]
  columnGenerator [] = []
  columnGenerator xs = (take 6 xs):(columnGenerator (drop 6 xs))
  
  
  
  {- 5. toFoundations -}
  
  toFoundations :: EOBoard -> EOBoard
  -- toFoundations ([],c,r) = ([[head (c !! index)]],(updateColumns c card),r) where (index,card) = scanForAces c
  toFoundations (f,c,r) = eOBoard

  updateColumns :: Columns -> Card -> Columns
  updateColumns [] c = []
  updateColumns (x:xs) c = (delete c x):(updateColumns xs c)
  
  updateReserve :: Reserve -> Card -> Reserve
  updateReserve res c = delete c res
  
  scanForCards :: [Deck] -> Card -> Deck
  scanForCards list found = filter (== (sCard found)) (map head list)
  
  scanForAces :: [Deck] -> Card
  scanForAces list = head (list !! (head (elemIndices Ace (map fst (map head list)))))
  
  
  {- filterColumn :: [Card] -> [Card] -> [Card]
  filterColumn list found = filter (\ x -> pCard x == (last found)) list
  
  addColumn :: [Card] -> [Card] -> [Card]
  addColumn [] _ = []
  addColumn (x:xs) found
    | null found = if p == Ace then (x):(addColumn xs [x]) else []
    | pCard x == (last found) = (x):(addColumn xs [x])
    | otherwise = addColumn xs found
    where (p,s) = x -}
  