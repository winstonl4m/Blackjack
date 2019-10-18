-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
import System.IO




import System.IO.Unsafe 

import System.Random
import Data.Array.IO
import Control.Monad




-- to run program: main

-- import Text.Read

-- -- algorithm for shuffle taken from https://wiki.haskell.org/Random_shuffle
-- -- | Randomly shuffle a list
-- -- /O(N)/

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


ranks = [1..13]

decks :: (Eq t, Num t) => t -> [Int]
decks 0 = []
decks n = ranks ++ ranks ++ ranks ++ ranks ++ decks (n-1)

-- accumulators/counters
-- each time draw card, update remaining cards
-- cards = n * 52 initial state

-- returns name of card
cardName :: (Eq a, Num a) => a -> [Char]
cardName 1 = "ACE"
cardName 2 = "TWO"
cardName 3 = "THREE"
cardName 4 = "FOUR"
cardName 5 = "FIVE"
cardName 6 = "SIX"
cardName 7 = "SEVEN"
cardName 8 = "EIGHT"
cardName 9 = "NINE"
cardName 10 = "TEN"
cardName 11 = "JACK"
cardName 12 = "QUEEN"
cardName 13 = "KING"


-- returns value of card
cardValue :: (Eq a, Num a, Enum a) => a -> a
cardValue n
    | n == 1 = 11 
    | n `elem` [2..10] = n
    | n `elem` [11..13] = 10
    | otherwise = 0

-- using Halves counting strategy from https://en.wikipedia.org/wiki/Card_counting 
-- returns score of card
-- cardScore :: Int -> Double
cardScore 1 = -1
cardScore 2 = 0.5
cardScore 3 = 1
cardScore 4 = 1
cardScore 5 = 1.5
cardScore 6 = 1
cardScore 7 = 0.5
cardScore 8 = 0
cardScore 9 = -0.5
cardScore 10 = -1
cardScore 11 = -1
cardScore 12 = -1
cardScore 13 = -1

data Result = EndOfGame Int State    -- end of game: value (0 = tie, 1 = house won, 2 = player won), next starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

data Action = Action (Int, Int)         -- (house, player) value indicates how many cards to draw

data State = State [Int] [Int] Double Double Double [Int] -- a state is a state where [list of house cards] [list of player cards] housefunds playerfunds bet [remaining cards in deck]
         deriving (Ord, Eq, Show)

type Game = State -> Result -- a game (blackjack) takes a state(current game), returns result tie=0, house win=1, player won=2, or continue

-- type Deal = State -> Action -> State -- take a state and deal cards 

blackjack :: State -> Result
blackjack (State houseCards playerCards houseFunds playerFunds bet currDeck) 
    | tie houseCards playerCards = EndOfGame 0 (State houseCards playerCards houseFunds (playerFunds + bet) 0 currDeck) -- player gets bet back
    | lose houseCards playerCards = EndOfGame 1 (State houseCards playerCards (houseFunds + bet) playerFunds 0 currDeck) 
    | win houseCards playerCards = EndOfGame 2 (State houseCards playerCards (houseFunds - 0.5*bet) (playerFunds + 1.5*bet) 0 currDeck)
    | otherwise = ContinueGame (State houseCards playerCards houseFunds playerFunds bet currDeck)
 
-- produce true if equal hands
--tie lst1 lst2 =
--    return (v1 == v2 && v1 >= 17 && v1 <= 21) -- house and player have equal hands, house won't hit if between 17 and 21 inclusive
--   where 
--        v1 = aceSum lst1
--        v2 = aceSum lst2

tie lst1 lst2 =
    if (aceSum lst1) == (aceSum lst2) && aceSum lst1 >= 17 && aceSum lst1 <= 21 
        then True 
        else False


-- produce true if player bust or house has higher hand and <=21
--lose lst1 lst2 =
--    return (v2 > 21 || (v1 <= 21 && v1 > v2)) 
--    where 
--        v1 = aceSum lst1
--        v2 = aceSum lst2

lose lst1 lst2 =
    if aceSum lst2 > 21 || aceSum lst1 <= 21 && aceSum lst1 > aceSum lst2 
        then True 
        else False

-- produce true if house bust or player has higher hand and <=21
--win lst1 lst2 =
--    return (v1 > 21 || (v2 <= 21 && v2 > v1)) 
--    where 
--        v1 = aceSum lst1
--        v2 = aceSum lst2

win lst1 lst2 =
    if aceSum lst1 > 21 || aceSum lst2 <= 21 && aceSum lst1 >= 17 && aceSum lst1 < aceSum lst2 
        then True 
        else False

-- return sum of cards and changes ace value (if there exist) to 1 if sum is over 21
aceSum lst = 
    if 1 `elem` lst && sum(map cardValue lst) > 21 
        then sum(map cardValue lst) - 10 
        else sum(map cardValue lst)

-- for (h,p), draw h cards for House, and p cards for player
drawCards :: State -> Action -> IO State
drawCards (State hCards pCards hFunds pFunds bet []) (Action (h,p)) =
    drawCards (State hCards pCards hFunds pFunds bet (genDeck 6)) (Action (h,p)) 
drawCards (State hCards pCards hFunds pFunds bet (f:r)) (Action (h,p))
    | p > 0 = drawCards (State hCards (pCards++[f]) hFunds pFunds bet r) (Action (h,p-1))
    | h > 0 = drawCards (State (hCards++[f]) pCards hFunds pFunds bet r) (Action (h-1,p))
    | otherwise = return (State hCards pCards hFunds pFunds bet (f:r))

genDeck n = unsafePerformIO (shuffle (decks n))


-- Taken from the TwentyOneQuestion assignment solution
-- getLineCorr reads the line and returns the corrected line by removing deleted characters
getLineCorr :: IO [Char]
getLineCorr =
  do 
    line <- getLine
    return (fixdel line)

-- fixdel str removes the character that precedes '\DEL' and '\DEL' itself in str and returns remaining str
fixdel :: [Char] -> [Char]
fixdel str =
  if '\DEL' `elem` str then fixdel (corr str) else str
-- *TwentyQs> fixdel "abc\DELd\DEL\DELefg\DELh"
-- "aefh"

-- corr (h:t) is the helper function for fixdel
corr :: [Char] -> [Char]
corr ('\DEL':t) = t
corr (h:'\DEL':t) = t
corr (h:t) = h:(corr t)

initState = State [] [] 100000000 0 0 []

-- set up bet and funds for player
setFundBet :: State -> IO State  
setFundBet (State hc pc hf pf bt d) = do
    putStrLn "Please enter your funds."
    f <- getLineCorr
    if isDouble f
        then do
        let funds = read f :: Double
        if funds < 50
            then do
                putStrLn "Invalid quantities. Please try again." 
                setFundBet (State hc pc hf pf bt d)
            else do
                putStrLn "Please enter your bet."  
                b <- getLineCorr
                if isDouble b
                    then do
                        let bet = read b :: Double
                        if bet <= funds && bet >= 50
                            then 
                                return (State hc pc hf (funds-bet) bet d) 
                            else do
                                putStrLn "Invalid quantities. Please try again." 
                                setFundBet (State hc pc hf pf bt d)
                    else do
                        putStrLn "Invalid input. Please try again."
                        setBet (State hc pc hf funds bt d)
        else do
            putStrLn "Invalid input. Please try again." 
            setFundBet (State hc pc hf pf bt d)

-- set bet for player given funds >=50
setBet :: State -> IO State
setBet (State hc pc hf pf bt d) = do
    putStrLn "Please enter your bet."  
    b <- getLineCorr
    if isDouble b 
        then do
            let bet = read b :: Double
            if bet <= pf && bet >= 50
                then do
                    return (State hc pc hf (pf-bet) bet d) 
                else do
                    putStrLn "Invalid quantity. Please try again." 
                    setBet (State hc pc hf pf bt d)
        else do
            putStrLn "Invalid input. Please try again."
            setBet (State hc pc hf pf bt d)

-- first round of dealing cards
firstCards :: State -> IO State
firstCards s = do
    s <- drawCards s (Action (1,2))
    putStrLn $ "Now dealing..." ++ "\n"
    putStrLn $ "House Cards: " ++ show (previewHC s) ++ "  Hand Value: " ++ show (hcValue s) ++ "\n"
    putStrLn $ "Player Cards: " ++ show (previewPC s) ++ "  Hand Value: " ++ show (pcValue s) ++ "\n"
    putStrLn $ "True Count: " ++ show (trueCount s) ++ "\n"
    return s

-- draw 1 card for player
drawPlayer :: State -> IO State
drawPlayer s = do
    s <- drawCards s (Action (0,1))
    return s

-- draw 1 card for house
drawHouse :: State -> IO State
drawHouse s = do
    s <- drawCards s (Action (1,0))
    return s

playerTurn :: State -> IO State
playerTurn s = do
    putStrLn "Hit or Stand?"
    ans <- getLineCorr
    if ans `elem` ["h","hit"]
        then do
            c <- drawPlayer s
            d <- result (blackjack c)
            let dval = resultVal (blackjack c)
            if dval `elem` [0,1,2]
                then do
                    e <- cleanState d
                    play e
                else
                    playerTurn d
        else if ans `elem` ["s","stand"]
            then do
                houseTurn s
            else do
                putStrLn "Invalid input. Please try again."
                putStrLn "Enter 'h' or 'hit' for HIT and 's' or 'stand' for STAND."
                playerTurn s

houseTurn :: State -> IO State
houseTurn s = do
    c <- drawHouse s
    d <- result (blackjack c)
    let dval = resultVal (blackjack d)
    if dval `elem` [0,1,2]
        then do
            e <- cleanState d
            play e
        else 
            houseTurn d

-- main :: IO ()
main = do
    play initState

play :: State -> IO State
play s = do
    putStrLn "Would you like to play Blackjack? The minimum bet is $50. House pays 3:2."
    ans <- getLineCorr
    if ans `elem` ["y", "yes"] 
        then do --check fund greater than 50 previewF >= 50
            -- a <- setFundBet s
            if (previewF s) >= 50 
                then do
                    a <- setBet s
                    b <- firstCards a
                    playerTurn b
                else do
                    a <- setFundBet s
                    b <- firstCards a
                    playerTurn b
    else if ans `elem` ["n","no"]
        then do 
            putStrLn "Thank you for visiting."
            -- return s
            return (State [] [] 0 0 0 [])
        else do
            putStrLn "Invalid input. Please try again."
            putStrLn "Enter 'y' or 'yes' for YES and 'n' or 'no' for NO."
            play s


-- show cards for house
previewHC (State hc pc hf pf bt d) = map cardName hc 
-- show cards for player
previewPC (State hc pc hf pf bt d) = map cardName pc 
-- show funds for player
previewF (State hc pc hf pf bt d) = pf
-- show value of player hand
pcValue (State hc pc hf pf bt d) = aceSum pc
-- show value of house hand
hcValue (State hc pc hf pf bt d) = aceSum hc

-- true count
trueCount :: State -> Double
trueCount (State hc pc hf pf bt d) =
    sum(map cardScore hc ++ map cardScore pc) / fromIntegral(numDeck2 d)


--total # of cards divided by # card in a deck (52) to get a ~ # of decks
numDeck :: [Int] -> Double
numDeck d = if (fromIntegral(length d)/ fromIntegral 52 - fromIntegral(length d `div` 52)) > 0.7 
                then 1
                else if (fromIntegral(length d)/ fromIntegral 52 - fromIntegral(length d `div` 52)) < 0.3
                    then 0
                    else 0.5


numDeck2 :: [Int] -> Int
numDeck2 d = if (fromIntegral(length d)/ fromIntegral 52 - fromIntegral(length d `div` 52)) == 0.5
                then fromIntegral(length d `div` 52) + 1
                else round(fromIntegral(length d) / fromIntegral 52)




resultVal :: Result -> Int
resultVal (EndOfGame n s) = n
resultVal (ContinueGame s) = 3

result :: Result -> IO State
result (EndOfGame 0 s) = do
    putStrLn ""
    putStrLn $ "House Cards: " ++ show (previewHC s) ++ "  Hand Value: " ++ show (hcValue s) ++ "\n"
    putStrLn $ "Player Cards: " ++ show (previewPC s) ++ "  Hand Value: " ++ show (pcValue s) ++ "\n"
    putStrLn $ "True Count: " ++ show (trueCount s) ++ "\n"
    putStrLn $ "-------------PUSH-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ show (previewF s) ++ "\n"
    return s
result (EndOfGame 1 s) = do
    putStrLn ""
    putStrLn $ "House Cards: " ++ show (previewHC s) ++ "  Hand Value: " ++ show (hcValue s) ++ "\n"
    putStrLn $ "Player Cards: " ++ show (previewPC s) ++ "  Hand Value: " ++ show (pcValue s) ++ "\n"
    putStrLn $ "True Count: " ++ show (trueCount s) ++ "\n"
    putStrLn $ "-------------LOST-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ show (previewF s) ++ "\n"
    return s
result (EndOfGame 2 s) = do
    putStrLn ""
    putStrLn $ "House Cards: " ++ show (previewHC s) ++ "  Hand Value: " ++ show (hcValue s) ++ "\n"
    putStrLn $ "Player Cards: " ++ show (previewPC s) ++ "  Hand Value: " ++ show (pcValue s) ++ "\n"
    putStrLn $ "True Count: " ++ show (trueCount s) ++ "\n"
    putStrLn $ "-------------WON-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ show (previewF s) ++ "\n"
    return s
result (ContinueGame s) = do
    putStrLn ""
    putStrLn $ "House Cards: " ++ show (previewHC s) ++ "  Hand Value: " ++ show (hcValue s) ++ "\n"
    putStrLn $ "Player Cards: " ++ show (previewPC s) ++ "  Hand Value: " ++ show (pcValue s) ++ "\n"
    putStrLn $ "True Count: " ++ show (trueCount s) ++ "\n"
    return s

-- clear all hands
cleanState (State hc pc hf pf bt d) = do
    return (State [] [] hf pf bt d)

-- taken from https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Haskell
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False



