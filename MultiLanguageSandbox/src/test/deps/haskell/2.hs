thirdLargest :: [Int] -> Int
thirdLargest xs = if length uniqueSorted >= 3 
                  then uniqueSorted !! 2 
                  else head uniqueSorted
  where
    uniqueSorted = reverse $ sort $ nub xs
check :: ([Int] -> Int) -> IO ()
check thirdLargestFunc = do
  let testCases =
        [ ([3, 2, 1], 1),
          ([1, 2], 2),
          ([10, 15, 5, 4, 20], 10),
          ([5], 5)
        ]

  mapM_ (\(numbers, expected) -> testCase numbers expected) testCases
  where
    testCase numbers expected = do
      let result = thirdLargestFunc numbers
      putStrLn $
        "Input: " ++ show numbers ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check thirdLargest