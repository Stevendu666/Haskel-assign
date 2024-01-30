import Data.Char
import System.Environment
import System.IO

main :: IO ()
main = do
  [filename, index] <- getArgs
  contents <- readFile filename
  writeFile (filename ++ ".chp") (encrypt (read index :: Int) contents)

  -- Decrypt the file
  let decryptedContents = c_decrypt (read index :: Int) contents
  writeFile (filename ++ ".decrypted") decryptedContents

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encrypt :: Int -> String -> String
encrypt n s = [shift n (toLower c) | c <- s]

decrypt :: Int -> String -> String
decrypt n s = encrypt (-n) s

c_decrypt :: Int -> String -> String
c_decrypt n s = decrypt n s