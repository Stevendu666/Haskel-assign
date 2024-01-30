import System.Random

-- Function to check if a point (x, y) is inside the quarter circle
isInsideQuarterCircle :: Double -> Double -> Bool
isInsideQuarterCircle x y = x^2 + y^2 <= 1

-- Function to perform the Monte Carlo simulation and estimate the area
monteCarloQuarterCircle :: Int -> IO Double
monteCarloQuarterCircle numPoints = do
    gen <- newStdGen
    let points = take numPoints $ randomPoints gen
        insidePoints = filter (\(x, y) -> isInsideQuarterCircle x y) points
        ratio = fromIntegral (length insidePoints) / fromIntegral numPoints
    return $ ratio * 1  -- Area of the square is 1

-- Function to generate random points in the square
randomPoints :: StdGen -> [(Double, Double)]
randomPoints gen = zip (randoms gen) (randoms (snd (split gen)))

main :: IO ()
main = do
    let numPoints = 1000000  -- Adjust the number of points as needed
    estimatedArea <- monteCarloQuarterCircle numPoints
    putStrLn $ "Estimated Area of the Quarter Circle: " ++ show estimatedArea
