module Main where

import Control.Monad  
import Data.Char
import qualified Data.List as List
import qualified Data.Map as Map
import System.Random

type IsSuited = Bool

data Rank = Two | Three | Four | Five | Six | Seven
          | Eight | Nine | Ten | Jack | Queen
          | King | Ace deriving (Enum, Eq, Ord, Bounded)
instance Show Rank where
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "T"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"
instance Random Rank where
  randomR (lo, hi) g = let (a, g') = randomR (fromEnum lo, fromEnum hi) g in (toEnum a, g')
  random = randomR (minBound, maxBound)

data Hand = Pair Rank | NonPair Rank Rank IsSuited
instance Show Hand where
    show (Pair r) = show r ++ show r
    show (NonPair r1 r2 s) = show r1 ++ show r2 ++ (if s then "s" else "o")

data RankSpan = RankSpan Rank (Maybe Rank)
type RangeForFirstCardFixed = RankSpan

type FirstRank = Rank
type Suited = Map.Map FirstRank RangeForFirstCardFixed
type Offsuit = Map.Map FirstRank RangeForFirstCardFixed
type Pairs = RankSpan
data Range = Range Suited Offsuit Pairs

type Position = Int

main = do
    spots <- generateSpots
    putStrLn "1 for fold, 2 for call, 3 for raise, 4 for 3bet/fold, 5 for 3bet/AI" 
    forM_  spots $ \spot@(position, hand) -> do
        let answer = isInRange (rfiRanges List.!! position) hand
        print spot 
        userans <- fmap read getLine
        putStrLn $ if isGoodAnswer userans answer then "Yes" else "No, correct answer is " ++ show answer
    where
        toHand (True, r1, _, _) = Pair r1
        toHand (False, r1, r2, s) = NonPair r1 r2 s

type UserAns = Int
type CorrectAns = Bool
isGoodAnswer :: Int -> Bool -> Bool
isGoodAnswer 3 True = True
isGoodAnswer 1 False = True
isGoodAnswer _ _ = False

generateSpots :: IO [(Position, Hand)]
generateSpots = do
    positions <- generatePositions
    hands <- generateHands
    return $ zip positions hands

generatePositions :: IO [Position]
generatePositions = do
    g <- newStdGen
    return (randomRs (0, length rfiRanges - 1) g :: [Position])

generateHands :: IO [Hand]
generateHands = do
    g <- newStdGen
    let isPair = map (\x -> x == 1) $ (randomRs (1, 6) g :: [Int])
    g' <- newStdGen
    let rank1 = randomRs (Two, Ace) g'
    g'' <- newStdGen
    let rank2 = randomRs (Two, Ace) g''
    g''' <- newStdGen
    let suit = randomRs (False, True) g'''
    return $ map toHand $ filter (\(_, rank1, rank2, _) -> rank1 > rank2) $ List.zip4 isPair rank1 rank2 suit
    where
        toHand (True, rank1, _, _) = Pair rank1
        toHand (False, rank1, rank2, suit) = NonPair rank1 rank2 suit

isInRange :: Range -> Hand -> Bool
isInRange rg h = case (rg, h) of
    (Range _ _ p, Pair r)                   ->  isInRankSpan p r
    (Range s o _, NonPair r1 r2 isSuited)   ->  if (Map.member r1 correctMap) then isInRankSpan (correctMap Map.! r1) r2 else False
                                                where correctMap = if isSuited then s else o
    where
        isInRankSpan (RankSpan mn Nothing) r = mn <= r
        isInRankSpan (RankSpan mn (Just mx)) r = mn <= r && r <= mx

utgRange :: Range
utgRange = Range suited offsuit pairs
    where
        suited = Map.fromList [(Ace, RankSpan Ten Nothing), (King, RankSpan Ten Nothing), (Queen, RankSpan Ten Nothing), (Jack, RankSpan Nine Nothing), (Ten, RankSpan Nine Nothing), (Nine, RankSpan Eight Nothing), (Eight, RankSpan Seven Nothing), (Seven, RankSpan Six Nothing)]
        offsuit = Map.fromList [(Ace, RankSpan Jack Nothing)]
        pairs = RankSpan Two Nothing
        
mpRange :: Range
mpRange = Range suited offsuit pairs
    where
        --TODO repair Ace rankspan after handling rankspan with gap (e.g. ATs+, A2-A5s)
        suited = Map.fromList [(Ace, RankSpan Two Nothing), (King, RankSpan Ten Nothing), (Queen, RankSpan Ten Nothing), (Jack, RankSpan Nine Nothing), (Ten, RankSpan Eight Nothing), (Nine, RankSpan Seven Nothing), (Eight, RankSpan Seven Nothing), (Seven, RankSpan Six Nothing)]
        offsuit = Map.fromList [(Ace, RankSpan Ten Nothing)]
        pairs = RankSpan Two Nothing
        
coRange :: Range
coRange = Range suited offsuit pairs
    where
        suited = Map.fromList [(Ace, RankSpan Two Nothing), (King, RankSpan Eight Nothing), (Queen, RankSpan Eight Nothing), (Jack, RankSpan Eight Nothing), (Ten, RankSpan Eight Nothing), (Nine, RankSpan Seven Nothing), (Eight, RankSpan Six Nothing), (Seven, RankSpan Five Nothing), (Six, RankSpan Five Nothing)]
        offsuit = Map.fromList [(Ace, RankSpan Eight Nothing), (King, RankSpan Ten Nothing), (Queen, RankSpan Ten Nothing), (Jack, RankSpan Ten Nothing)]
        pairs = RankSpan Two Nothing
rfiRanges :: [Range]
rfiRanges = [utgRange, mpRange, coRange]