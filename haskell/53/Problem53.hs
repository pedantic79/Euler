import qualified Data.Bimap as BM
import Data.List (sort,sortBy)
import Data.Ord (compare)

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

isStraight :: [Card] -> Bool
isStraight cs = if last cs' == 14
                then check cs' || check ace
                else check cs'
  where cs' = map cardValue . sort $ cs
        ace = 1:init cs'
        check xs@(x:_) = [x .. x+4] == xs
