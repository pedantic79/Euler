import qualified Data.Bimap as BM

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
  readsPrec _ xxs@(x:xs) =
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

instance Show Rank where
  show Ace = "A"
  show King = "K"
  show Queen = "Q"
  show Jack = "J"
  show Ten = "T"
  show Nine = "9"
  show Eight = "8"
  show Seven = "7"
  show Six = "6"
  show Five = "5"
  show Four = "4"
  show Three = "3"
  show Two = "2"  
                   
