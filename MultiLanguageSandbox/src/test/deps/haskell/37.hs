averageOfDivisibleAndNonDivisible :: Int -> Int -> (Float, Float)
averageOfDivisibleAndNonDivisible n k =
    let divisible = [x | x <- [1..n], x `mod` k == 0]
        nonDivisible = [x | x <- [1..n], x `mod` k /= 0]
        avgA = if null divisible then 0 else fromIntegral (sum divisible) / fromIntegral (length divisible)
        avgB = if null nonDivisible then 0 else fromIntegral (sum nonDivisible) / fromIntegral (length nonDivisible)
    in (avgA, avgB)
check :: (Int -> Int -> (Float, Float)) -> IO ()
check averageFunc = do
  let testCases =
        [ ((10, 2), (6.0, 5.0)),
          ((15, 5), (10.0, 7.5)),
          ((20, 4), (12.0, 10.0)),
          ((9, 3), (6.0, 4.5))
        ]

  mapM_ (\((n, k), (expectedA, expectedB)) -> testCase n k expectedA expectedB) testCases
  where
    testCase n k expectedA expectedB = do
      let (resultA, resultB) = averageFunc n k
      putStrLn $
        "Input: " ++ show (n, k) ++
        ", Expected A: " ++ show expectedA ++ ", Result A: " ++ show resultA ++
        ", Expected B: " ++ show expectedB ++ ", Result B: " ++ show resultB ++
        if (resultA, resultB) == (expectedA, expectedB) then " (Pass)" else error "Test failed. Exiting..."
main :: IO ()
main = check averageOfDivisibleAndNonDivisible