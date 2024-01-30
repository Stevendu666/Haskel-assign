import Data.Char (toLower, isLower)  --Q2
import Data.List (sort, group)  --Q2 
import System.Environment ( getArgs ) --Q5
import Data.Function --Q10
import System.IO
import Data.Char
import System.Environment



--Q1：

is_square :: Int -> Bool
is_square x 
   | x < 0 = False
   | otherwise = (floor (sqrt (fromIntegral x)))^2 == x


--Q2：

freq_letter_pc :: String -> [(Char, Float)]

freq_letter_pc text =
  let lowercaseText = map toLower (filter isLower text)
      totalChars = fromIntegral (length lowercaseText)
      charGroups = group (sort lowercaseText)
  in
    [(char, (fromIntegral (length groupChars) / totalChars) ) | groupChars@(char:_) <- charGroups]


--Q3：

type City = (Int, String, Int, Int)  -- city_id, city_name, city_population, country_id
type Country = (Int, String)         -- country_id, country_name

-- data
cities :: [City]
cities = [(1,"Paris",7000000,1),(2,"London",8000000,2),(3,"Rome",3000000,3),
          (4,"Edinburgh",500000,2),(5,"Florence",50000,3),(6,"Venice",200000,3),
          (7,"Lyon",1000000,1),(8,"Milan",3000000,3),(9,"Madrid",6000000,4),
          (10,"Barcelona",5000000,4)]

countries :: [Country]
countries = [(1,"UK"), (2,"France"), (3,"Italy"), (4,"Spain")]

-- Function a: get_city_above
get_city_above :: Int -> [String]
get_city_above n = [cityName | (_, cityName, cityPop, _) <- cities, cityPop >= n]

-- Function b: get_city
get_city :: String -> [String]
get_city countryName =
  let countryId = case lookupCountryId countryName of
                    Just id -> id
                    Nothing -> error "Country not found"
  in [cityName | (_, cityName, _, cityCountryId) <- cities, cityCountryId == countryId]

-- Helper function to lookup country_id by country_name
lookupCountryId :: String -> Maybe Int
lookupCountryId countryName = lookup countryName [(countryName', countryId) | (countryId, countryName') <- countries]

-- Function c: num_city
num_city :: [(String, Int)]
num_city = [(countryName, countCities countryId) | (countryId, countryName) <- countries]

-- Helper function to count cities in a country
countCities :: Int -> Int
countCities countryId = length [() | (_, _, _, cityCountryId) <- cities, cityCountryId == countryId]


--Q4：

eucl_dist :: Floating a => [a] -> [a] -> a
-- eucl_dist x y = sqrt $ sum (map (\(a,b) -> (a-b)^2) (zip x y))

eucl_dist x y = sqrt $ sum (map (\(a,b) -> (a-b)^2) (zip x y))


--Q5：

-- Function to compute the Euclidean distance between two frequency distributions
eucl_dist2 :: Floating a => [a] -> [a] -> a
eucl_dist2 x y
    | length x /= length y = error "Vectors must have the same number of elements"
    | otherwise = sqrt $ sum $ map (\(a, b) -> (a - b) ^ 2) (zip x y)

-- Function to identify the language of a given text
get_lang :: String -> IO ()
get_lang text = do
    let lowerText = map toLower text
        eng_freq = [8.12,1.49,2.71,4.32,12.02,2.30,2.03,5.92,7.31,0.10,0.69,3.98,2.61,6.95,7.68,1.82,0.11,6.02,6.28,9.10,2.88,1.11,2.09,0.17,2.11,0.07]
        pt_freq = [12.21,1.01,3.35,4.21,13.19,1.07,1.08,1.22,5.49,0.30,0.13,3.00,5.07,5.02,10.22,3.01,1.10,6.73,7.35,5.07,4.46,1.72,0.05,0.28,0.04,0.45]
        text_freq = map (\c -> fromIntegral (length $ filter (== c) lowerText) / fromIntegral (length lowerText) * 100) ['a'..'z']
        dist_to_eng = eucl_dist text_freq eng_freq
        dist_to_pt = eucl_dist text_freq pt_freq

    putStrLn $ if dist_to_eng < dist_to_pt
        then "The text is in English"
        else "The text is in Portuguese"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            get_lang contents
        _ -> putStrLn "Usage: ./lang filename"


--Q6：

-- Please see the c_decrypt.hs file


--Q7：

-- Part 1: Please see Dict.hs file
-- Part 2: Please see guessIndex.hs file


--Q8：

-- Please see the randomPoints.hs file

--Q9：

-- Please see the math_series.hs file

--Q10：

-- Please see the integral.hs file
