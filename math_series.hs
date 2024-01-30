import System.Environment (getArgs)

-- Higher-order function to compute the sum of the first n elements of a series
math_series :: (Float -> Float) -> Int -> Float
math_series series n = sum $ take n $ map series [1..]

-- Define the sample_series function for 1/2^k
sample_series :: Float -> Float 
sample_series k = 1 / (2**k)

-- Define the series ∑ (-1)^(k+1) * (4/(2k-1))
pi_series :: Float -> Float
pi_series k = (-1)**(k+1) * (4 / (2*k - 1))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [seriesName, nStr] -> do
            let n = read nStr :: Int
                result = case seriesName of
                    "sample_series" -> math_series sample_series n
                    "pi_series" -> math_series pi_series n
                    _ -> error "Unknown series function"
            putStrLn $ "Sum of the first " ++ show n ++ " elements of " ++ seriesName ++ " series: " ++ show result
        _ -> putStrLn "Usage: ./program series_name number_of_terms"


-- math_series :: (Int -> Float) -> Int -> Float
-- math_series series n = sum [series k | k <- [1..n]]


-- -- Example series
-- sample_series :: (Fractional a, Integral b) => b -> a
-- sample_series k = 1 / 2^k

-- piSeries :: (Integral a, Fractional a) => a -> a
-- piSeries k = ((-1)^(k+1)) * (4/(2*k-1))

-- main = do
--   print $ math_series sample_series 4 




