daysToLengthOne :: Int -> Int
daysToLengthOne 1 = 1
daysToLengthOne a = 1 + daysToLengthOne (a `div` 2)
check :: (Int -> Int) -> IO ()
check daysToLengthOneFunc = do
  let testCases =
        [ (100, 7),
          (50, 6),
          (25, 5),
          (10, 4),
          (5, 3),
          (3, 2),
          (1, 1)
        ]

  mapM_ (\(length, expectedDays) -> testCase length expectedDays) testCases
  where
    testCase length expectedDays = do
      let result = daysToLengthOneFunc length
      putStrLn $
        "Initial Length: " ++ show length ++
        ", Expected Days: " ++ show expectedDays ++ ", Result: " ++ show result ++
        if result == expectedDays then " (Pass)" else error "Test failed. Exiting..." ++ show result
      

main :: IO ()
main = check daysToLengthOne