import System.IO
import System.Environment
import Data.Char (toLower, isAlpha, ord, chr)
import Data.List (nub, maximumBy)
import Data.Ord (comparing)

-- Function to extract distinct unique words from a given text
extractWords :: String -> [String]
extractWords text = nub $ filter (not . null) $ map (map toLower . filter isAlpha) $ words text

-- Function to decrypt a message with a given index
decrypt :: Int -> String -> String
decrypt n s = [shift (-n) c | c <- s]
  where
    shift n c
      | toLower c `elem` ['a'..'z'] = int2let ((let2int (toLower c) + n) `mod` 26)
      | otherwise = c

    let2int :: Char -> Int
    let2int c = ord c - ord 'a'

    int2let :: Int -> Char
    int2let n = chr (ord 'a' + n)

-- Function to count the number of English words in a text
countEnglishWords :: [String] -> String -> Int
countEnglishWords dictionary text = length $ filter (`elem` dictionary) $ extractWords text

-- Function to guess the index by maximizing the number of English words
guessIndex :: [String] -> String -> Int
guessIndex dictionary encryptedText =
    fst $ maximumBy (comparing snd) [(i, countEnglishWords dictionary $ decrypt i encryptedText) | i <- [0..25]]

main :: IO ()
main = do
    [filename] <- getArgs
    encryptedText <- readFile filename
    dictionary <- lines <$> readFile "dict.txt"

    let index = guessIndex dictionary encryptedText
    putStrLn $ "Guessed Index: " ++ show index

    -- Use decrypt function
    let decryptedContents = decrypt index encryptedText
    writeFile (filename ++ ".decrypted") decryptedContents

