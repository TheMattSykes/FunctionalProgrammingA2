module EightOff where
  
  import MergeSort
  import System.Random
  import Data.List
  
  {- 1. Haskell Datatypes -}
  
  data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Enum, Eq, Ord)
  
  data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Enum, Eq, Ord)
  
  type Card = (Pip,Suit)
  
  type Deck = [Card] -- List of cards
  
  type EOBoard = (Foundations,Columns,Reserve)
  
  -- Foundations contain 4 decks of cards starting with Ace and ending with King. Must be same suit in each foudnation.
  type Foundations = [Deck]
  
  -- 8 Columns which are decks of cards
  type Columns = [Deck]
  
  -- 8 Reserves which can contain a card each or be null
  type Reserve = [Card]
  
  
  
  {- 2. Haskell constants and utilities -}
  
  pack :: Deck
  -- List comprehention: combindes the pips and suits into unique pairs to form a deck of 52 cards.
  pack = [(pip,suit) | pip <- [Ace ..], suit <- [Clubs ..]]
  
  -- Increases the Pip value of the entered card
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
  
  randomList = take 56 (randoms (mkStdGen 20) :: [Int])
  zippedList = zip pack randomList
  sortedList = mergesort (\(_,n1) (_,n2)->n1<n2) zippedList
  
  shuffle = map fst sortedList
  
  
  
  {- 4. eODeal -}
  
  eOBoard :: EOBoard
  eOBoard = ([[(Ace,Spades)]],col,res)
  
  res :: Reserve
  res = take 4 shuffle
  
  col :: Columns
  col = columnGenerator (drop 4 shuffle)
  
  columnGenerator :: Eq a => [a] -> [[a]]
  columnGenerator [] = []
  columnGenerator xs = (take 6 xs):(columnGenerator (drop 6 xs))
  
  
  
  {- 5. toFoundations -}
  
  toFoundations :: EOBoard -> EOBoard
  -- toFoundations ([],c,r) = ([[card]], updateColumns c card, updateReserve r card) 
    -- where card = scanForAces (c `union` reservesToElements r)
  toFoundations (f,c,r)
    | null cardA && null cardB = (f,c,r)
    | null cardB = toFoundations (updateFoundations f cardA, updateColumns c cardA, updateReserve r cardA)
    | otherwise = toFoundations (updateFoundations f cardB, updateColumns c cardB, updateReserve r cardB)
    where cardA = scanForAces (c `union` res)
          cardB = if null cardBl then [] else (head cardBl)
          cardBl = scanForCards (c `union` res) f
          res = reservesToElements r
  
  reservesToElements :: Eq a => [a] -> [[a]]
  reservesToElements [] = []
  reservesToElements (x:xs) = [x]:(reservesToElements xs)
  
  updateFoundations :: Foundations -> Card -> Foundations
  updateFoundations [] c = if p == Ace then [[c]] else [] where (p,s) = c
  updateFoundations (x:xs) c
    | p == Ace && x == [] = [c]:xs
    | c == sCard (last x) = (x ++ [c]):xs
    | otherwise = x:(updateFoundations xs c)
    where (p,s) = c
  
  updateColumns :: Columns -> Card -> Columns
  updateColumns [] c = []
  updateColumns (x:xs) c = (delete c x):(updateColumns xs c)
  
  updateReserve :: Reserve -> Card -> Reserve
  updateReserve res c = delete c res
  
  scanForCards :: [Deck] -> Foundations -> [Card]
  scanForCards list [] = []
  scanForCards list (x:xs)
    | null filtered = scanForCards list xs
    | otherwise = [head filtered]
    where filtered = (filter (== (sCard (last x))) (map head list))
  
  scanForAces :: [Deck] -> Card
  scanForAces list = head (list !! (head (elemIndices Ace (map fst (map head list)))))
  