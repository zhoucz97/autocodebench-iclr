sumSeries :: Int -> Int
sumSeries n = sumHelper n 0
  where
    sumHelper 0 acc = acc
    sumHelper k acc = sumHelper (k - 1) (acc + k)
check :: (Int -> Int) -> IO ()
check sumSeriesFunc = do
  let testCases =
        [ (5, 15),
          (10, 55),
          (20, 210),
          (1, 1),
          (50, 1275)
        ]

  mapM_ (\(n, expected) -> testCase n expected) testCases
  where
    testCase n expected = do
      let result = sumSeriesFunc n
      putStrLn $
        "Input: " ++ show n ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check sumSeries