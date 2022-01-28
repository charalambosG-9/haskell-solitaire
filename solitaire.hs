{- 
Charalambos Georgiades
Implementation of an auto-player for the games eight-off solitaire 
and spider solitaire
-}

--Imports

import System.Random
import Data.List
import Data.Maybe

--Type definitions

data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Show, Enum, Ord)

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving ( Eq, Show, Enum, Ord )

type Card = (Pip,Suit) 

type Deck = [Card]

type Foundations = [Card]  

type Column = [Card] 

type Reserves = [Card]

type Stock = [Card]

type SColumn = [(Card,Bool)]

data Board = EOBoard Foundations [Column] Reserves | SBoard Foundations [SColumn] Stock deriving Eq

--Functions

--Creates a pack of cards
pack :: [Card]
pack = [(x,y) | x <- [Ace .. King], y <- [Hearts .. Diamonds]]

--Finds the successor of a given card
sCard :: Card -> Card
sCard (pip,suit) = if pip == King then (Ace,suit) else (succ pip,suit);

--Finds the predecessor of a given card
pCard :: Card -> Card
pCard (pip,suit) = if pip == Ace then (King,suit) else (pred pip,suit);

--Checks if a given card is an Ace
isAce :: Card -> Bool
isAce (pip,suit) = if pip == Ace then True else False

--Checks if a given card is a King
isKing :: Card -> Bool
isKing (pip,suit) = if pip == King then True else False

--Compares two cards (used in the shuffle function)
cmp :: Ord a => (Card,a) -> (Card,a) -> Ordering
cmp (x1, y1) (x2, y2) = compare y1 y2

--Shuffles a deck of cards based on the seed provided 
shuffle ::Int -> Deck -> Deck
shuffle seed unshuffledDeck = [ newDeck | (newDeck, n) <- sortBy cmp (zip unshuffledDeck (randoms(mkStdGen seed) :: [Int]))] 

--Instance show for the two types of boards
instance Show Board where
    show (EOBoard foundations columns reserves) = "EOBoard \nFoundations "  
     ++ show foundations 
     ++ "\nColumns \n" 
     ++ showColumns columns   
     ++ "Reserves " ++ show reserves
    show (SBoard foundations scolumns stock) = "SBoard \nFoundations "
     ++ show foundations
     ++ "\nColumns \n"
     ++ showSColumns scolumns
     ++ "Stock " ++ show ((length stock) `div` 10) ++ " Deals remaining"

-- A function for displaying the columns of an eight-off Board, one per line
showColumns [] = ""
showColumns (x:xs) = show x ++ "\n" ++ showColumns xs 

-- A function for displaying the columns of a spider Board, one per line
showSColumns [] = ""
showSColumns (x:xs) = "[" ++ checkWholeColumn x ++ "\n" ++ showSColumns xs
        where
            checkWholeColumn [] = "\b]"
            checkWholeColumn (y:ys)
                | (snd y) == True = show (fst y) ++ "," ++ checkWholeColumn ys
                | otherwise = "<unknown>," ++ checkWholeColumn ys 

--The initial board of a game of eight-off solitaire as shown in appendix A
initialEOBoard = EOBoard 
                  []
                  [[(Ace,Clubs),(Seven,Diamonds),(Ace,Hearts),(Queen,Hearts),(King,Spades),(Four,Spades)],
                  [(Five,Diamonds),(Queen,Spades),(Three,Diamonds),(Five,Spades),(Six,Spades),(Seven,Hearts)],
                  [(King,Hearts),(Ten,Diamonds),(Seven,Spades),(Queen,Diamonds),(Five,Hearts),(Eight,Diamonds)],
                  [(Jack,Spades),(Six,Hearts),(Seven,Clubs),(Eight,Spades),(Ten,Clubs),(Queen,Clubs)],
                  [(Ace,Spades),(Eight,Clubs),(Ace,Diamonds),(King,Diamonds),(Jack,Hearts),(Four,Clubs)],
                  [(Two,Diamonds),(Three,Hearts),(Three,Clubs),(Ten,Hearts),(Six,Diamonds),(Jack,Clubs)],
                  [(Nine,Spades),(Four,Diamonds),(Nine,Clubs),(Nine,Hearts),(Three,Spades),(Ten,Spades)],
                  [(Two,Clubs),(Two,Spades),(Four,Hearts),(Nine,Diamonds),(King,Spades),(Eight,Hearts)]]
                  [(Two,Hearts),(Six,Clubs),(Five,Clubs),(Jack,Diamonds)]

--An in progress board of a game of spider solitaire as shown in appendix B
inProgressSBoard = SBoard [(King, Hearts)] 
                       [[((Eight, Diamonds), True),((Nine, Hearts), True)],
                       [((Two,Diamonds), True)],
                       [((Ace, Spades), True), ((Two, Spades), True), ((Three, Spades), True), ((Four, Spades), True),
                        ((Five, Spades), True), ((Six, Clubs), True), ((Seven, Clubs), True), ((Eight, Clubs), True),
                        ((Nine, Clubs), True), ((Ten, Diamonds), True), ((Jack, Diamonds), True), ((Queen, Diamonds), True),
                        ((King, Diamonds), True), ((Four,Clubs), False), ((Eight,Hearts), False)],
                       [((Seven, Clubs), True), ((Eight, Diamonds), True), ((Nine, Diamonds), True), ((Ten, Diamonds), True),
                        ((Jack, Diamonds), True),((Queen, Diamonds), True), ((King, Diamonds), True), ((Nine, Clubs), True),
                        ((Ten, Hearts), True), ((Jack, Clubs), True)],
                       [((Ace, Hearts), True), ((Two, Hearts), True), ((Three, Hearts), True), ((Four, Hearts), True),
                        ((Five, Hearts), True), ((Six, Diamonds), True), ((Seven, Diamonds), True), 
                        ((Queen, Clubs), True), ((King, Hearts), True)],
                       [((Two, Diamonds), True), ((Three, Diamonds), True) ,((Four, Diamonds), True)],
                       [((Jack, Clubs), True), ((Queen, Clubs), True), ((King, Clubs), True), ((Two, Spades), True),
                        ((Three, Spades), True), ((Four, Diamonds), True),((Five, Diamonds), True), ((Six, Diamonds), True),
                        ((Seven, Hearts), True), ((Eight, Clubs), True), ((Nine, Spades), True), ((Ten, Clubs), True),
                        ((Ace, Clubs), True), ((Two, Clubs), True), ((Three, Clubs), True), ((Four, Clubs), True), ((Five, Spades), True)],
                       [((Seven, Spades), True), ((Eight, Spades), True), ((Nine, Spades), True), ((Ten, Spades), True),
                        ((Jack, Spades), True), ((Queen, Spades), True), ((King, Spades), True), ((Ace,Diamonds), False),
                        ((Six, Spades), False), ((King,Spades), False)],
                       [((Jack, Hearts), True), ((Queen, Hearts), True)],
                       [((Ace, Clubs), True), ((Two, Clubs), True)]]
                       [(Five,Clubs),(Six,Spades), (Six,Hearts), (Seven,Diamonds), (Ten,Clubs), (Jack,Spades),
                        (Three,Diamonds), (Three,Clubs), (Ten,Spades), (Five,Clubs), (Queen,Spades), (Ace,Spades),
                        (Nine,Diamonds), (Seven,Spades), (Six,Clubs), (King,Clubs), (Ace,Diamonds), (Four,Spades),(Eight,Spades), (Five,Diamonds)]      

--Deals a random initial board of eight-off solitaire
eoDeal :: Int -> Board
eoDeal seed = EOBoard [] (makeColumns shuffledPack) (drop 48 shuffledPack)
    where shuffledPack = shuffle seed pack

--Helper function for creating the columns for an initial board of eight-off solitaire
makeColumns :: Deck -> [Column]
makeColumns [] = []
makeColumns pack
  | length pack < 6 = []
  | otherwise = (take 6 pack) : (makeColumns (drop 6 pack))

--Deals a random initial board of spider solitaire
sDeal :: Int -> Board
sDeal seed = SBoard [] (turnFirstTrue(makeSColumns xs)) (drop 54 xs)
    where xs = shuffle seed (pack++pack)

--Creates the columns of an initial board of spider solitaire, with all of the cards in the columns face down
makeSColumns :: Deck -> [SColumn]
makeSColumns [] = []
makeSColumns pack
  | length pack < 54 = []
  | length pack < 80 = (makeColumn (take 5 pack)) : (makeSColumns (drop 5 pack))
  | otherwise = (makeColumn (take 6 pack)) : (makeSColumns (drop 6 pack)) 
    where
        makeColumn :: Deck -> SColumn
        makeColumn [] = []
        makeColumn (x:xs) = [(x,False)] ++ makeColumn xs

--Helper function that turns the first card in every column of an initial board of spider solitaire, face up
turnFirstTrue :: [SColumn] -> [SColumn]
turnFirstTrue [] = []
turnFirstTrue ((y:ys):xs) = ([(fst y,True)] ++ ys) : turnFirstTrue xs
  
--Function that moves all possible cards to the foundations in a game of eight-off solitaire
toFoundations :: Board -> Board
toFoundations (EOBoard foundations columns reserves) = checkColumns columns []
    where 
        checkColumns :: [Column] -> [Column] -> Board
        checkColumns [] ys = checkReserves reserves ys
        checkColumns (x:xs) ys
            | x == [] = checkColumns xs ys
            | isAce (head x) = toFoundations (EOBoard (head x:foundations) (ys ++ tail x:xs) reserves)
            | length (filter (\c -> sCard c == (head x)) foundations) /= 0 = toFoundations (EOBoard (replaceCard (head x) foundations) (ys ++ tail(x):xs) reserves)
            | otherwise = checkColumns xs (ys++[x])
        checkReserves :: Reserves -> [Column] -> Board
        checkReserves [] ys = (EOBoard foundations ys reserves)
        checkReserves (x:xs) ys
            | isAce x = toFoundations (EOBoard (x:foundations) ys (removeCard x reserves))
            | length (filter (\c -> sCard c == x) foundations) /= 0 = toFoundations (EOBoard (replaceCard (x) foundations ) ys (removeCard x reserves))
            | otherwise = checkReserves xs ys
--Function that moves all possible cards to the foundations in a game of spider solitaire
toFoundations (SBoard foundations scolumn stock) = checkSColumn scolumn []
    where 
    checkSColumn :: [SColumn] -> [SColumn] -> Board
    checkSColumn [] ys = (SBoard foundations ys stock)
    checkSColumn (x:xs) ys
        | x == [] = checkSColumn xs ys 
        | isAce(fst(head(x))) && (checkSingleSColumn x 1) = toFoundations(SBoard (kingOfSuit(fst(head(x))):foundations) (ys++[ms]++xs) stock) 
        | otherwise = checkSColumn xs (ys ++ [x])
        where 
            ms = removeElements x 13
            checkSingleSColumn :: SColumn -> Int -> Bool
            checkSingleSColumn (x:xs) count
                | count == 13 = True
                | length xs == 0 = False
                | fst(head(xs)) == sCard(fst(x)) && snd(x) && snd(head(xs))= checkSingleSColumn xs (count+1)
                | otherwise = False
            kingOfSuit :: Card -> Card
            kingOfSuit (pip,suit) = (King,suit)
            removeElements :: SColumn -> Int -> SColumn
            removeElements (x:xs) 1 = xs
            removeElements (x:xs) count = removeElements xs (count-1) 

--Helper function for replacing a card (adding a card on top of another) used in toFoundations for eight-off solitaire
replaceCard :: (Card) -> [(Card)] -> [(Card)]
replaceCard newCard = map (\x -> if (sCard x == newCard) then newCard else x)

--Helper function for removing a card from a list of cards (mainly the reserves) used in toFoundations for eight-off solitaire
removeCard :: (Card) -> [(Card)] -> [(Card)]
removeCard card list = filter (\e -> e /= card) list

--Returns all possible board states of a game of eight-off solitaire after a single move
findMoves :: Board -> [Board]
findMoves (EOBoard foundations columns reserves) = [(toFoundations (EOBoard foundations columns reserves))] ++ sortByPriority (EOBoard foundations columns reserves) ( (reservesToCols (EOBoard foundations columns reserves) [] []) ++ (colsToCols (EOBoard foundations columns reserves) []) ++ (colsToReserves (EOBoard foundations columns reserves) [])) []
--Returns all possible board states of a game of spider solitaire after a single move
findMoves (SBoard foundations scolumn stock) = [toFoundations (SBoard foundations scolumn stock)] ++ multipleCards (SBoard foundations scolumn stock) [] ++ singleSColumnToSColumn (SBoard foundations scolumn stock) [] ++ stockToSCols (SBoard foundations scolumn stock) [] 

--Functions used in findMoves for a board of eight-off solitaire

--Function that sorts the list returned from the colsToReserves function from most optimal to least optimal
sortByPriority :: Board -> [Board] -> [Board] -> [Board]
sortByPriority (EOBoard foundations columns reserves) [x] ys = [x]
sortByPriority (EOBoard foundations columns reserves) xs ys = diffScore (EOBoard foundations columns reserves) xs []
    where 
    --Function that returns the first board which has more cards in the foundations, if there is one
    diffScore :: Board -> [Board] -> [Board] -> [Board]
    diffScore m [] ys = moreColumns (EOBoard foundations columns reserves) ys []
    diffScore (EOBoard foundations columns reserves) ((EOBoard nfoundations ncolumns nreserves):xs) ys 
        | foundations /= nfoundations = [(EOBoard nfoundations ncolumns nreserves)]
        | otherwise = diffScore (EOBoard foundations columns reserves) xs ((EOBoard nfoundations ncolumns nreserves):ys)
    --Function  that returns the first board which has placed a king in an empty column, if there is one
    moreColumns :: Board -> [Board] -> [Board] -> [Board]
    moreColumns m [] ys = notToReserves (EOBoard foundations columns reserves) ys []
    moreColumns (EOBoard foundations columns reserves) ((EOBoard nfoundations ncolumns nreserves):xs) ys 
        | checkForKing columns ncolumns = [(EOBoard nfoundations ncolumns nreserves)]
        | otherwise = moreColumns (EOBoard foundations columns reserves) xs ((EOBoard nfoundations ncolumns nreserves):ys)
        where
            checkForKing :: [Column] -> [Column] -> Bool
            checkForKing _ [] = False
            checkForKing [] _ = False
            checkForKing   (x:xs) (y:ys)
                | length x == 1 && length y == 1 && fst (head x) == King && fst (head y) == King = False
                | length y == 1 && length x == 0 && fst (head y) == King = True 
                | otherwise = checkForKing xs ys
    --Function that returns the first board for which a move was not made from a column to the reserves, if there is one
    notToReserves :: Board -> [Board] -> [Board] -> [Board]
    notToReserves m [] ys = toResAndBack (EOBoard foundations columns reserves) ys []
    notToReserves (EOBoard foundations columns reserves) ((EOBoard nfoundations ncolumns nreserves):xs) ys 
        | length nreserves <= length reserves = [(EOBoard nfoundations ncolumns nreserves)]
        | otherwise = notToReserves (EOBoard foundations columns reserves) xs ((EOBoard nfoundations ncolumns nreserves):ys)
    --Function that returns the first board for which a move is made to the reserve and can be placed in a new column, if there is one
    toResAndBack :: Board -> [Board] -> [Board] -> [Board]
    toResAndBack m [] ys = nextMove (EOBoard foundations columns reserves) ys
    toResAndBack (EOBoard foundations columns reserves) ((EOBoard nfoundations ncolumns nreserves):xs) ys 
        | length (reservesToCols (EOBoard nfoundations ncolumns nreserves) [] []) /= 0 && not (elem (EOBoard foundations columns reserves) (reservesToCols (EOBoard nfoundations ncolumns nreserves) [] [])) = [(EOBoard nfoundations ncolumns nreserves)]
        | otherwise = toResAndBack (EOBoard foundations columns reserves) xs ((EOBoard nfoundations ncolumns nreserves):ys)
    --Function that returns the first board for which a move is made to the reserve and cannot be placed on the same column, if there is one
    nextMove :: Board -> [Board] -> [Board]
    nextMove m [] = []
    nextMove (EOBoard foundations columns reserves) ((EOBoard nfoundations ncolumns nreserves):xs) 
        | elem (EOBoard foundations columns reserves) (reservesToCols (EOBoard nfoundations ncolumns nreserves) [] []) = nextMove (EOBoard foundations columns reserves) xs 
        | otherwise = [(EOBoard nfoundations ncolumns nreserves)]


--Function that finds all possibe moves from column to reserve in a game of eight-off solitaire
colsToReserves :: Board -> [Column] -> [Board]
colsToReserves (EOBoard foundations [] reserves) ys = []
colsToReserves (EOBoard foundations (x:xs) reserves) ys
            | length reserves < 8 = if length x /= 0 then toFoundations(EOBoard foundations (ys++[(tail x)]++xs) (head x:reserves)) : colsToReserves (EOBoard foundations xs reserves) (ys++[x]) else colsToReserves (EOBoard foundations xs reserves) (ys++[x])
            | otherwise = []

--Function that finds all possible moves from reserves to columns in a game of eight-off solitaire
reservesToCols :: Board -> [Column] -> [Card] -> [Board]
reservesToCols (EOBoard foundations [] reserves) cs ls = []
reservesToCols (EOBoard foundations (y:ys) []) cs ls = reservesToCols (EOBoard foundations ys ls) (cs ++ [y]) []
reservesToCols (EOBoard foundations (y:ys) (x:xs)) cs ls 
    | isKing(x) =  if length y == 0 then toFoundations(EOBoard foundations (cs++[[x]]++ys) (ls++xs)) : (reservesToCols (EOBoard foundations ys (ls++(x:xs))) (cs++[y]) []) else reservesToCols (EOBoard foundations (y:ys) xs) cs (ls ++ [x])
    | length y /= 0 = if sCard(x) == (head y) then toFoundations(EOBoard foundations (cs++[(x:y)]++ys) (ls++xs)) : (reservesToCols (EOBoard foundations ys (ls++(x:xs))) (cs++[y]) []) else reservesToCols (EOBoard foundations (y:ys) xs) cs (ls ++ [x])
    | otherwise =  reservesToCols (EOBoard foundations (y:ys) xs) cs (ls ++ [x])

--Function that finds all possible moves from a column to another column in a game of eight-off solitaire
colsToCols:: Board -> [Column] -> [Board]
colsToCols (EOBoard foundations [] reserves) cs = []
colsToCols (EOBoard foundations (y:ys) reserves) cs 
    | length y /= 0 && isKing(head y) = if length (moveKing y ds []) /= 0 then toFoundations(EOBoard foundations (moveKing y ds []) reserves) : colsToCols (EOBoard foundations ys reserves) (cs++[y]) else colsToCols (EOBoard foundations ys reserves) (cs++[y])
    | length y /= 0 = if length (moveSucc y ds []) /= 0 then toFoundations(EOBoard foundations (moveSucc y ds []) reserves) : colsToCols (EOBoard foundations ys reserves) (cs++[y]) else colsToCols (EOBoard foundations ys reserves) (cs++[y])
    | otherwise = colsToCols (EOBoard foundations ys reserves) (cs++[y])
    where ds = (cs ++ (y:ys))

--Function that moves a king from a column to an empty column in a game of eight-off solitaire
moveKing :: Column -> [Column] -> [Column] -> [Column]
moveKing (x:xs) [] cs = []
moveKing (x:xs) (d:ds) cs 
    | length d == 0 = (ns ++ [[x]] ++ ms) 
    | otherwise = moveKing (x:xs) ds (cs++[d])
    where ms = replaceCol (x:xs) ds
          ns = replaceCol (x:xs) cs
          replaceCol :: Column -> [Column] -> [Column]
          replaceCol col = map (\x -> if ( length x /= 0 && head x == head col) then tail x else x)

--Function that moves a predecessor from a column to another column in a game of eight-off solitaire
moveSucc :: Column -> [Column] -> [Column] -> [Column]
moveSucc (x:xs) [] cs = []
moveSucc (x:xs) (d:ds) cs 
    | length d /= 0 && head d == sCard x = (ns ++ [x:d] ++ ms) 
    | otherwise = moveSucc (x:xs) ds (cs++[d])
    where ms = replaceCol (x:xs) ds
          ns = replaceCol (x:xs) cs
          replaceCol :: Column -> [Column] -> [Column]
          replaceCol col = map (\x -> if ( length x /= 0 && head x == head col) then tail x else x)


--Functions used in findMoves for a board of spider solitaire

--Function that moves the first ten cards from the stock to the columns in a game of spider solitaire
stockToSCols :: Board -> [SColumn] -> [Board]
stockToSCols (SBoard foundations [] stock) passedCols = [toFoundations(SBoard foundations passedCols stock)]
stockToSCols (SBoard foundations (x:xs) (y:ys)) passedCols
    | length x == 0 = []
    | otherwise = stockToSCols (SBoard foundations xs ys) (passedCols ++ ms) 
    where ms = [((y,True):x)]

--Function that moves a single card from one scolumn to another
singleSColumnToSColumn :: Board -> [SColumn] -> [Board]
singleSColumnToSColumn (SBoard foundations [] stock) cs = [] 
singleSColumnToSColumn (SBoard foundations (y:ys) stock) cs 
    | length y /= 0 = if length (moveSSucc y ds []) /= 0 then singleMakeSCols (SBoard foundations (y:ys) stock) (moveSSucc y ds []) ++ singleSColumnToSColumn (SBoard foundations ys stock) (cs++[y]) else singleSColumnToSColumn (SBoard foundations ys stock) (cs++[y])
    | otherwise = singleSColumnToSColumn (SBoard foundations ys stock) (cs++[y]) 
    where ds = (cs ++ (y:ys))

--Helper function for singleSColumnToSColumn that tests each column 
moveSSucc :: SColumn -> [SColumn] -> [SColumn] -> [[SColumn]] 
moveSSucc (x:xs) [] cs = []
moveSSucc (x:xs) (d:ds) cs 
    | length d /= 0 && fst(fst x) /= King && fst(fst(head d)) == succ(fst(fst x)) = [(ns ++ [x:d] ++ ms)] ++ moveSSucc (x:xs) ds (cs++[d])
    | length d == 0 && length xs /= 0 = [(ns ++ [[x]] ++ ms)] ++ moveSSucc (x:xs) ds (cs)
    | otherwise = moveSSucc (x:xs) ds (cs++[d])
    where ms = replaceCol (x:xs) ds
          ns = replaceCol (x:xs) cs
          replaceCol :: SColumn -> [SColumn] -> [SColumn]
          replaceCol col = map (\x -> if (length x /= 0 && x == col) then flipCard(tail x) else x) --replaces both version of a card if both are head length x /= 0 && head x == head col
          flipCard :: SColumn -> SColumn
          flipCard [] = []
          flipCard [x] = [(fst x, True)]
          flipCard (x:xs) = (fst x, True):xs 

--Function that moves the most amount of cards from one column to another
multipleCards :: Board -> [SColumn] -> [Board]
multipleCards (SBoard foundations [] stock) cs = [] 
multipleCards (SBoard foundations (y:ys) stock) cs  
    | length y /= 0 = if length (moveMany y [head y] ds []) /= 0 then singleMakeSCols (SBoard foundations (y:ys) stock) (moveMany y [head y] ds []) ++ multipleCards (SBoard foundations ys stock) (cs++[y]) else multipleCards (SBoard foundations ys stock) (cs++[y])
    | otherwise = multipleCards (SBoard foundations ys stock) (cs++[y])
    where ds = (cs ++ (y:ys))

--Helper function for multipleCards that tests each column
moveMany :: SColumn -> SColumn -> [SColumn] -> [SColumn] -> [[SColumn]]
moveMany (x:xs) stack [] cs = []
moveMany (x:xs) stack (d:ds) cs 
    | length xs /= 0 && snd(head(xs)) == True && sCard(fst x) == fst(head xs) = moveMany xs ((head xs):stack) (d:ds) cs 
    | length d /= 0 && fst(fst(head(stack))) /= King && (fst(fst(head d))) == succ(fst(fst(head(stack)))) = [(ns ++ [(reverse(stack)++d)] ++ ms)] ++ moveMany (x:xs) stack ds (cs++[d])
    | length d == 0 && length xs /= 0 = [(ns ++ [(reverse stack)] ++ ms)] ++  moveMany (x:xs) stack ds cs
    | otherwise = moveMany (x:xs) stack ds (cs++[d])
    where ms = replaceCol (x:xs) stack ds
          ns = replaceCol (x:xs) stack cs
          replaceCol :: SColumn -> SColumn -> [SColumn] -> [SColumn]
          replaceCol col stack = map (\x -> if (length x /= 0 && head x == head (reverse stack)) then flipCard(removeCards x (length stack)) else x)
          flipCard :: SColumn -> SColumn
          flipCard [] = []
          flipCard [x] = [(fst x, True)]
          flipCard (x:xs) = (fst x, True):xs 
          removeCards :: SColumn -> Int -> SColumn
          removeCards (x:xs) counter 
            | counter /= 1 = removeCards xs (counter-1)
            | otherwise = xs 

--Helper function that makes a list of spider boards based on all possible moves given
singleMakeSCols :: Board -> [[SColumn]] -> [Board]
singleMakeSCols (SBoard foundations [] stock) _ = []
singleMakeSCols (SBoard foundations scolumn stock) [] = []
singleMakeSCols (SBoard foundations scolumn stock) (y:ys) = [toFoundations(SBoard foundations y stock)] ++ singleMakeSCols (SBoard foundations scolumn stock) ys


--Chooses which move to make next in a game of eight-off solitaire
chooseMove :: Board -> Maybe Board
chooseMove (EOBoard foundations columns reserves)  
    | length(findMoves(EOBoard foundations columns reserves)) == 1 && head(findMoves(EOBoard foundations columns reserves)) == (EOBoard foundations columns reserves) = Nothing
    | head(findMoves(EOBoard foundations columns reserves)) == (EOBoard foundations columns reserves) = Just (head(tail(findMoves(EOBoard foundations columns reserves))))
    | otherwise = Just (head(findMoves(EOBoard foundations columns reserves)))
--Chooses which move to make next in a game of spider solitaire
chooseMove (SBoard foundations columns stock)
    | length(findMoves(SBoard foundations columns stock)) == 1 && findMoves(SBoard foundations columns stock) == [(SBoard foundations columns stock)] = Nothing
    | head(findMoves(SBoard foundations columns stock)) /= (SBoard foundations columns stock) = Just (head(findMoves(SBoard foundations columns stock)))
    | length (tail(findMoves(xs))) /= 0 && (SBoard foundations columns stock) == head(tail(findMoves(xs))) = Nothing
    | otherwise = Just (head(tail(findMoves(SBoard foundations columns stock))))
    where xs = (head(tail(findMoves(SBoard foundations columns stock))))

--Plays a game of eight-off solitaire and returns the score
playSolitaire :: Board -> Int
playSolitaire (EOBoard foundations columns reserves) 
    | haveWon(EOBoard foundations columns reserves) = 52
    | chooseMove(EOBoard foundations columns reserves) == Nothing = findScore foundations 0
    | otherwise = playSolitaire(fromJust(chooseMove(EOBoard foundations columns reserves)))
--Plays a game of spider solitaire and returns the score
playSolitaire (SBoard foundations columns stock) 
    | haveWon(SBoard foundations columns stock) = 104
    | chooseMove(SBoard foundations columns stock) == Nothing = findScore foundations 0
    | otherwise = playSolitaire(fromJust(chooseMove(SBoard foundations columns stock)))

--Finds the score when supplied the foundations of a board
findScore :: [Card] -> Int -> Int
findScore [] s = s
findScore (x:xs) s
    | fst x == Ace = (findScore xs (s+1))
    | fst x == Two = (findScore xs (s+2))
    | fst x == Three = (findScore xs (s+3))
    | fst x == Four = (findScore xs (s+4))
    | fst x == Five = (findScore xs (s+5))
    | fst x == Six = (findScore xs (s+6))
    | fst x == Seven = (findScore xs (s+7))
    | fst x == Eight = (findScore xs (s+8))
    | fst x == Nine = (findScore xs (s+9))
    | fst x == Jack = (findScore xs (s+11))
    | fst x == Queen = (findScore xs (s+12))
    | fst x == King = (findScore xs (s+13))
    | otherwise = (findScore xs (s+10))   
        
--Function that checks if the board supplied is a winning board
haveWon :: Board -> Bool
haveWon (EOBoard foundations [] []) = True
haveWon (EOBoard foundations columns []) = False
haveWon (EOBoard foundations [] reserves) = False
haveWon (EOBoard foundations columns reserves) = False
haveWon (SBoard foundations [] []) = True
haveWon (SBoard foundations columns []) = False
haveWon (SBoard foundations [] stock) = False
haveWon (SBoard foundations columns stock) = False

--Function that takes a seed and the numbers of games of eight-off solitaire and returns the number of games won and the average score of all games 
analyseEO :: Int -> Int -> (Int,Float)
analyseEO seed n = getAvg seed b b x y
    where b = n
          x = 0
          y = 0
          getAvg :: Int -> Int -> Int -> Int -> Int -> (Int,Float)
          getAvg seed gamesToPlay 0 wins totalScore = (wins, fromIntegral (totalScore) / fromIntegral (gamesToPlay))
          getAvg seed gamesToPlay gamesLeft wins totalScore
            | play == 52 = getAvg (seed+1) gamesToPlay (gamesLeft-1) (wins+1) (totalScore+52)
            | otherwise = getAvg (seed+1) gamesToPlay (gamesLeft-1) wins (totalScore+play)
            where play = playSolitaire (eoDeal seed)


--Function that takes a seed and the numbers of games of spider solitaire and returns the number of games won and the average score of all games 
analyseSpider :: Int -> Int -> (Int,Float)
analyseSpider seed n = getAvg seed b b x y
    where b = n
          x = 0
          y = 0
          getAvg :: Int -> Int -> Int -> Int -> Int -> (Int,Float)
          getAvg seed gamesToPlay 0 wins totalScore = (wins, fromIntegral (totalScore) / fromIntegral (gamesToPlay))
          getAvg seed gamesToPlay gamesLeft wins totalScore 
            | play == 104 = getAvg (seed+1) gamesToPlay (gamesLeft-1) (wins+1) (totalScore+104)
            | otherwise = getAvg (seed+1) gamesToPlay (gamesLeft-1) wins (totalScore+play)
            where play =  playSolitaire (sDeal seed)


  {- Paste the contents of this file, including this comment, into your source file, below all
     of your code. You can change the indentation to align with your own, but other than this,
     ONLY make changes as instructed in the comments.
   -}
  -- Constants that YOU must set:
studentName = "Charalambos Georgiades"
studentNumber = "200130291"
studentUsername = "acb19cg"

initialBoardDefined = initialEOBoard {- replace XXX with the name of the constant that you defined
                               in step 3 of part 1 -}
secondBoardDefined = inProgressSBoard {- replace YYY with the constant defined in step 5 of part 1,
                              or if you have chosen to demonstrate play in a different game
                              of solitaire for part 2, a suitable contstant that will show
                              your play to good effect for that game -}

  {- Beyond this point, the ONLY change you should make is to change the comments so that the
     work you have completed is tested. DO NOT change anything other than comments (and indentation
     if needed). The comments in the template file are set up so that only the constant eight-off
     board from part 1 and the toFoundations function from part 1 are tested. You will probably
     want more than this tested.

     CHECK with Emma or one of the demonstrators if you are unsure how to change this.

     If you mess this up, your code will not compile, which will lead to being awarded 0 marks
     for functionality and style.
  -}

main :: IO()
main =
    do
      putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

      putStrLn "***The eight-off initial board constant from part 1:"
      print initialBoardDefined

      let board = toFoundations initialBoardDefined
      putStrLn "***The result of calling toFoundations on that board:"
      print board

      {- Move the start comment marker below to the appropriate position.
        If you have completed ALL the tasks for the assignment, you can
        remove the comments from the main function entirely.
        DO NOT try to submit/run non-functional code - you will receive 0 marks
        for ALL your code if you do, even if *some* of your code is correct.
      -}

      

      let boards = findMoves board      -- show that findMoves is working
      putStrLn "***The possible next moves after that:"
      print boards

      let chosen = chooseMove board     -- show that chooseMove is working
      putStrLn "***The chosen move from that set:"
      print chosen

      putStrLn "***Now showing a full game"     -- display a full game
      score <- displayGame initialBoardDefined 0
      putStrLn $ "Score: " ++ score
      putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)


      putStrLn "\n\n\n************\nNow looking at the alternative game:"

      putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
      print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                        -- is not an initial game, but a point from which the game
                                        -- can be won

      putStrLn "***Now showing a full game for alternative solitaire"
      score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                                -- works correctly)
      putStrLn $ "Score: " ++ score
      putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)


      {- start comment marker - move this if appropriate
      -}

  {- displayGame takes a Board and move number (should initially be 0) and
     displays the game step-by-step (board-by-board). The result *should* be
     the same as performing playSolitaire on the initial board, if it has been
     implemented correctly.
     DO NOT CHANGE THIS CODE other than aligning indentation with your own.
  -}
displayGame :: Board -> Int ->IO String
displayGame board n =
    if haveWon board
      then return "A WIN"
      else
        do
          putStr ("Move " ++ show n ++ ": " ++ show board)
          let maybeBoard = chooseMove board
          if isJust maybeBoard then
            do
              let (Just newBoard) = maybeBoard
              displayGame newBoard (n+1)
          else
            do
              let score = show (playSolitaire board)
              return score


