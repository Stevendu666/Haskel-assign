-- Function to approximate the integral of a given function
integral :: (Float -> Float) -> Float -> Float -> Int -> Float
integral f x1 x2 n = sum [f (x1 + fromIntegral i * delta) * delta | i <- [0..n-1]]
  where
    delta = (x2 - x1) / fromIntegral n

-- Example function f: f x = 0.5 * x
exampleFunction :: Float -> Float
exampleFunction x = 0.5 * x

main :: IO ()
main = do
    let result1 = integral exampleFunction 1 20 9
        result2 = integral exampleFunction 1 20 10000

    putStrLn $ "Integral result with 9 intervals: " ++ show result1
    putStrLn $ "Integral result with 10000 intervals: " ++ show result2

