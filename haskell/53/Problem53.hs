import qualified Data.Bimap as BM
import Data.List (group,sort,sortBy)

data PokerHands = HighCard
                | OnePair
                | TwoPair
                | ThreeKind
                | Straight
                | Flush
                | FullHouse
                | FourKind
                | StraightFlush
                deriving (Enum, Eq, Ord, Bounded, Show)

data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
          deriving (Enum, Eq, Ord, Bounded)

suitStrMap = BM.fromList [ (Spades, "S")
                         , (Hearts, "H")
                         , (Diamonds, "D")
                         , (Clubs, "C")
                         ]

instance Show Suit where
  show s = suitStrMap BM.! s

instance Read Suit where
  readsPrec _ (x:xs) =
    case BM.lookupR [x] suitStrMap of
     Nothing -> []
     Just n  -> [(n, xs)]

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King            
          | Ace
          deriving (Enum, Eq, Ord, Bounded)

rankStrMap = BM.fromList [ (Ace, "A")
                         , (King, "K")
                         , (Queen, "Q")
                         , (Jack, "J")
                         , (Ten, "T")
                         , (Nine, "9")
                         , (Eight, "8")
                         , (Seven, "7")
                         , (Six, "6")
                         , (Five, "5")
                         , (Four, "4")
                         , (Three, "3")
                         , (Two, "2")
                         ]

instance Show Rank where
  show r = rankStrMap BM.! r

instance Read Rank where
  readsPrec _ (x:xs) =
    case BM.lookupR [x] rankStrMap of
     Nothing -> []
     Just n  -> [(n, xs)]

data Card = Card { rank :: Rank
                 , suit :: Suit
                 } deriving (Eq, Ord)

instance Show Card where
  show (Card r s) = show r ++ show s

instance Read Card where
  readsPrec _ (r:s:xs) =
    let r' = read [r]
        s' = read [s]
    in [(Card r' s', xs)]



parseFile :: FilePath -> IO [[Card]]
parseFile f = do
  fh <- readFile f
  let w = map (map read . words) . lines $ fh
  return w

cardValue c = fromEnum (rank c) + 2

revSort = sortBy (flip compare)

computeValue :: [Card] -> Int
computeValue cs = foldl (\a c -> a * 100 + c) 0 cs'
  where cs' = concat . getGroup $ cs

isFlush :: [Card] -> Bool
isFlush (c:cs) =
  let s = suit c
  in all (==s) $ map suit cs

mapCV = map cardValue . sort
mapRCV = map cardValue . revSort

isStraight cs = check $ mapCV cs
  where check xs@(x:_) = [x..x+4] == xs

getGroup = sortBy (\x y -> compare (length y) (length x)) . group . mapRCV

matchGroup (x,y) cs =
  let (a:b:_) = getGroup cs
  in length a == x && length b == y

isFullHouse = matchGroup (3,2)

isFourKind = matchGroup (4,1)

isThreeKind = matchGroup (3,1)

isTwoPair = matchGroup (2,2)

isPair = matchGroup (2,1)

getHand :: [Card] -> PokerHands
getHand cs
  | isFlush cs && isStraight cs = StraightFlush
  | isFourKind cs = FourKind
  | isFullHouse cs = FullHouse
  | isFlush cs = Flush
  | isStraight cs = Straight
  | isThreeKind cs = ThreeKind
  | isTwoPair cs = TwoPair
  | isPair cs = OnePair
  | otherwise = HighCard

getHandValue :: [Card] -> (PokerHands, Int)
getHandValue cs = (getHand cs, computeValue cs)

evalCards :: [Card] -> Bool
evalCards cs =
  let (player1, player2) = splitAt 5 cs
      (hand1, val1) = getHandValue player1
      (hand2, val2) = getHandValue player2
  in if hand1 == hand2
     then val1 > val2
     else hand1 > hand2

evalFile :: FilePath -> IO Int
evalFile f = do
  allCards <- parseFile f
  let trueMap = map evalCards allCards
  return . length . filter id $ trueMap

main = evalFile "p054_poker.txt" >>= print
