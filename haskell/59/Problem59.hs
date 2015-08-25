import Data.Bits (xor)
import Data.Char (chr,ord)
import Data.List (permutations)
import qualified Data.Text as T

xorData key = zipWith xor (concat . repeat $ key)

keys = [map ord [a, b, c] | a <- ['a' .. 'z']
                          , b <- ['a' .. 'z']
                          , c <- ['a' .. 'z']]

decrypt n =  map (map chr . (`xorData` n)) keys


checkStr s = filt s == T.empty
  where
    filt = T.filter check . T.pack
    check c = not . all ($c) $ [(>=' '), (<='z'), (/='/'), (/= '`')]

problem59 = sum . map ord . head . filter checkStr . decrypt

readData :: FilePath -> IO [Int]
readData fp = do
  f <- readFile fp
  return . read $ "[" ++ f ++ "]"

-- 107359
main = readData "p059_cipher.txt" >>= print . problem59
  
      
