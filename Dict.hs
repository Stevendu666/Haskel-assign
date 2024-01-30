import Data.Char (toLower, isAlpha)
import Data.List (nub, sort)
import System.IO

-- Function to extract distinct unique words from a given text
extractWords :: String -> [String]
extractWords text = nub $ filter (not . null) $ map (map toLower . filter isAlpha) $ words text

-- Function to read novels and create a dictionary file
buildDictionary :: IO ()
buildDictionary = do
    prideText <- readFile "pride.txt"
    ulyssesText <- readFile "ulysses.txt"
    dorianText <- readFile "dorian.txt"
    
    let dictionary = sort $ extractWords $ prideText ++ " " ++ ulyssesText ++ " " ++ dorianText
    
    writeFile "dict.txt" $ unlines dictionary

main :: IO ()
main = buildDictionary