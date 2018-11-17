module EightOff where
  
  import MergeSort
  import System.Random
  import Data.List
  
  {- 1. Haskell Datatypes ------------------------------------ -}
  
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
  
  
  
  {- 2. Haskell constants and utilities ------------------------------------ -}
  
  pack :: Deck
  -- List comprehention: combindes the pips and suits into unique pairs to form a deck of 52 cards.
  pack = [(pip,suit) | pip <- [Ace ..], suit <- [Clubs ..]]
  
  -- Increases the Pip value of the entered card
  sCard :: Card -> Card
  sCard (a,b) = (succ a,b)
  
  -- Decreases the Pip value of the entered card
  pCard :: Card -> Card
  pCard (a,b) = (pred a,b)
  
  -- Checks if the Pip value (first part of tuple) is an Ace
  isAce :: Card -> Bool
  isAce (a,_) = a == Ace
  
  -- Checks if the Pip value (first part of tuple) is a King
  isKing :: Card -> Bool
  isKing (a,_) = a == King
  
  
  
  {- 3. Shuffle Function ------------------------------------ -}
  
  -- Take a list of 56 random numbers using a generator seed
  randomList = take 56 (randoms (mkStdGen 64) :: [Int])
  -- Map the random numbers to the cards in the pack
  zippedList = zip pack randomList
  -- Sort the random numbers (and cards) in order
  sortedList = mergesort (\(_,n1) (_,n2)->n1<n2) zippedList
  
  -- Remove random numbers from list but retain the shuffled order i.e. now a list of shuffled cards
  shuffle = map fst sortedList
  
  
  
  {- 4. eODeal ------------------------------------ -}
  
  -- Return a default EOBoard with empty foundations
  eODeal :: EOBoard
  eODeal = ([],col,res)
  
  -- Take the first 4 shuffled cards for the reserves
  res :: Reserve
  res = take 4 shuffle
  
  -- Use the rest of the shuffled cards to form the columns
  col :: Columns
  col = columnGenerator (drop 4 shuffle)
  
  -- Generate columns in groups of six cards each
  columnGenerator :: Eq a => [a] -> [[a]]
  columnGenerator [] = []
  columnGenerator xs = (take 6 xs):(columnGenerator (drop 6 xs))
  
  
  
  {- 5. toFoundations ------------------------------------ -}
  
  {- toFoundations
     Takes and returns an EOBoard.
     First creates cardA which uses scanForAces, is either Nothing or an Ace card.
     Next creates cardB which uses scanForCards, is either Nothing or an successor card for a foundation.
     Function then checks cases and when no more successors or aces are found it will return the EOBoard.
  -}
  toFoundations :: EOBoard -> EOBoard
  toFoundations (f,c,r) = 
    -- Check if cards are a set value or Nothing
    case cardB of 
      Nothing -> 
        case cardA of
          -- No more cards are available for foundations
          Nothing -> (f,c,r)
          -- An Ace has been found on the front of a column or reserve
          Just cardA -> toFoundations (updateFoundations f cardA, removeEmptyCols (updateColumns c cardA), updateReserve r cardA)
      -- A successor to a card on the foundations has been found, EOBoard updated accordingly
      Just cardB -> toFoundations (updateFoundations f cardB, removeEmptyCols (updateColumns c cardB), updateReserve r cardB)
    where cardA = scanForAces (c `union` (reservesToElements r)) -- Join columns and reserves together
          cardB = scanForCards (c `union` (reservesToElements r)) f
  
  -- reservesToElements: Seperates every reserve card into its own list e.g. [(),()] = [[()],[()]]
  reservesToElements :: Eq a => [a] -> [[a]]
  reservesToElements [] = []
  reservesToElements (x:xs) = [x]:(reservesToElements xs)
  
  -- updateFoundations: Adds a card to the sequence on a foundation if it is a successor for that foundation
  updateFoundations :: Foundations -> Card -> Foundations
  updateFoundations [] c = if p == Ace then [[c]] else [] where (p,s) = c
  updateFoundations (x:xs) c
    | p == Ace && x == [] = [c]:xs -- If Ace add to start of a new foundation
    -- If not a king, if the card at the top of the foundation is the input card then add to foundation.
    | not (isKing (last x)) = if c == sCard (last x) then (x ++ [c]):xs else x:(updateFoundations xs c)
    | otherwise = x:(updateFoundations xs c)
    where (p,s) = c -- Split card into pip and suit values pattern
  
  -- updateColumns: Attempts to find the input card and removes it from the column
  updateColumns :: Columns -> Card -> Columns
  updateColumns [] c = [] -- Base case
  updateColumns (x:xs) c = (delete c x):(updateColumns xs c)
  
  -- updateReserve: Removes the input card from the reserves
  updateReserve :: Reserve -> Card -> Reserve
  updateReserve res c = delete c res
  
  -- removeEmptyCols: Removes columns that have no cards inside, used to prevent empty list errors.
  removeEmptyCols :: Columns -> Columns
  removeEmptyCols [] = [] -- Base case
  removeEmptyCols (x:xs)
    | null x = removeEmptyCols xs -- If null then remove column
    | otherwise = x:(removeEmptyCols xs)
  
  -- scanForCards: Searches list for successor cards to those on the foundations
  scanForCards :: [Deck] -> Foundations -> Maybe Card
  scanForCards list [] = Nothing -- Returns Nothing if no cards found
  scanForCards list (x:xs)
    | null filtered = scanForCards list xs -- Nothing found on this foundation, move to next
    | otherwise = Just (head filtered) -- Return found successor card
    -- If foundation not completed then filter successor cards from foundation
    where filtered = if (isKing (last x)) then [] else (filter (== (sCard (last x))) (map head list))
  
  -- scanForAces: Searches list for Ace cards and returns if one exists
  scanForAces :: [Deck] -> Maybe Card
  scanForAces list
    | null newList = Nothing -- No Ace found return Nothing
    | otherwise = Just (head (list !! (head (newList)))) -- Ace Found, retreive from list indices of Ace cards found
    -- Forms a list of all Ace locations on the top of columns/reserves, returns list of indices
    where newList = elemIndices Ace (map fst (map head list))
  