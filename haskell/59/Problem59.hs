import Data.Bits (xor)
import Data.Char (ord)

xorData key = zipWith xor (concat . repeat $ key)

keys = [map ord [a, b, c] | a <- ['a' .. 'z']
                          , b <- ['a' .. 'z']
                          , c <- ['a' .. 'z']]

decrypt n = map (`xorData` n) keys

checkStr = all check
  where                --  ' '     'z'      '/'     '`'
    check c = all ($c) [(>=32), (<=122), (/=47), (/=96)]

problem59 = sum . head . filter checkStr . decrypt

readData :: FilePath -> IO [Int]
readData fp = do
  f <- readFile fp
  return . read $ "[" ++ f ++ "]"

-- 107359/0.08s
main = readData "p059_cipher.txt" >>= print . problem59

