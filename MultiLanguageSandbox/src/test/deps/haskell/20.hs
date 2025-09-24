minOperationsToIncrease :: [Int] -> Int
minOperationsToIncrease [] = 0
minOperationsToIncrease [_] = 0
minOperationsToIncrease (x:xs) = go xs x 0
  where
    go [] _ acc = acc
    go (y:ys) prev acc
      | y > prev = go ys y acc
      | otherwise = go ys (prev + 1) (acc + (prev + 1 - y))
check :: ([Int] -> Int) -> IO ()
check minOperationsToIncreaseFunc = do
  let testCases =
        [ ([1,2,3], 0),
          ([1,1,1], 3),
          ([1,5,2,4,1], 14),
          ([8,5,9,2,6], 20),
          ([3], 0)
        ]

  mapM_ (\(nums, expected) -> testCase nums expected) testCases
  where
    testCase nums expected = do
      let result = minOperationsToIncreaseFunc nums
      putStrLn $
        "Input: nums = " ++ show nums ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check minOperationsToIncrease