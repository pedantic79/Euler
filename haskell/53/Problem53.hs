import qualified Data.Bimap as BM
import Data.List (group,sort,sortBy)
import Data.Ord (compare)

data PokerHands = HighCard
                | Pair
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

cardValue (Card r _) = fromEnum r + 2

revSort = sortBy (flip compare)

computeValue :: [Card] -> Int
computeValue cs = foldl (\a c -> a * 100 + cardValue c) 0 $ revSort cs

isFlush :: [Card] -> Bool
isFlush (c:cs) =
  let s = suit c
  in all (==s) $ map suit cs

mapCV = map cardValue . sort
mapRCV = map cardValue . revSort

isStraight :: [Card] -> (Bool, Int)
isStraight cs = if check cs'
                then (True, lcard)
                else if last cs' == 14
                     then (check ace, last ace)
                     else (False, lcard)
  where cs' = mapCV cs
        ace = 1:init cs'
        lcard = last cs'
        check xs@(x:_) = [x .. x+4] == xs

getGroup = sortBy (\x y -> compare (length y) (length x)) . group . mapRCV

isFullHouse cs =
  let (t:p:gs) = getGroup cs
  in length t == 3 && length p == 2

isFourKind cs =
  let (f:gs) = getGroup cs
  in length f == 4

isThreeKind cs =
  let (t:gs) = getGroup cs
  in length t == 3

isTwoPair cs =
  let (s:t:gs) = getGroup cs
  in length s == 2 && length t == 2

isPair cs =
  let (s:t:gs) = getGroup cs
  in length s == 2 && length t == 1

isNone cs =
  let (s:gs) = getGroup cs
  in length s == 1

getHandValue cs
  | isFlush cs && isStr = (StraightFlush, strVal)
  | isFourKind cs = (FourKind, cv)
  | isFullHouse cs = (FullHouse, cv)
  | isFlush cs = (Flush, cv)
  | isStr = (Straight, strVal)
  | isThreeKind cs = (ThreeKind, cv)
  | isTwoPair cs = (TwoPair, cv)
  | isPair cs = (TwoPair, cv)
  | otherwise = (HighCard, cv)
  where (isStr, strVal) = isStraight cs
        gs = getGroup cs
        cv = computeValue cs
